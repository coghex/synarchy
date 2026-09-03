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
     in the dormant season. Poked via world.setDate. Then (#1711) the
     regrowth CYCLE on that same tile: a harvest starts a positive
     timer, an immediate second harvest is refused while it runs, and
     only an actual game-time tick (not a calendar poke — the tick
     decrements by dtGame) reopens the tile. Both species under
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

EVERY file this invocation creates — the two fixture YAMLs, the engine
log, and that resource root with its saves — lives under one directory
this process owns, and goes away again on every handled exit (#1682).
Before that the two fixtures and the log were fixed `/tmp` names no run
cleaned up, which two concurrent runs collided on. `--keep-artifacts`
retains the directory instead, and names it, for diagnosing a failure.

Usage: python3 tools/flora_growth_probe.py [--port 9186] [--seed 42]
       [--size 64] [--plates 3] [--keep-artifacts]
"""
import argparse, glob, os, shutil, socket, stat, subprocess, sys, tempfile, \
       time, uuid
from probelib import (FixtureNotRegistered, boot, capture_request_id,
                      load_fixture_yaml, quit_engine, send, send_json,
                      wait_load_published, wait_save_complete)

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def _make_owner_writable(top: str) -> None:
    """Add owner write (and directory search) permission throughout a
    freshly copied tree.

    `shutil.copytree` reproduces the SOURCE's mode bits, so a checkout
    whose `config/` is read-only — a CI cache restored read-only, a
    read-only mount, an archive unpacked without write bits — yields a
    private `config/` this run cannot use and cannot delete: a directory
    needs owner write+search before any of its entries can be unlinked,
    so `release_artifacts` would report residue and leave the whole tree
    behind on a run that did nothing wrong. That matters more since
    #1682 than it did under #1616, because the tree now holds this
    run's fixtures and engine log as well as its saves. The copy is THIS
    invocation's, so it is made writable regardless of what the source
    happened to be; the source itself is never touched. Same treatment
    `tools/location_embark_probe.py` gives its own copy (#1569).
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


class RunArtifacts:
    """Every file one invocation of this probe creates, under a single
    directory that invocation owns (#1682).

    `base` comes from `tempfile.mkdtemp`, so it is this process's alone
    and disjoint from every other invocation's — which is what makes the
    logical names inside it (`engine.log`, `probe_berry.yaml`,
    `probe_clover.yaml`) safe to keep fixed. Two concurrent runs on
    distinct `--port` values therefore write two disjoint trees: neither
    can overwrite the other's fixture between its write and the
    engine-side read of it, and neither can truncate the other's log.
    `tools/run_probes.py --jobs N` and `tools/probe_flake.py`'s
    machine-wide port lease both make that concurrency a supported mode,
    not a hypothetical.

    Nothing this process did not create is ever named, so a file of the
    same name outside the tree — a developer's own `/tmp/probe_berry.yaml`
    — is not opened for writing, truncated, modified or removed. Before
    #1682 the two fixtures and the engine log were exactly those fixed
    `/tmp` names, written with a truncating `open(..., "w")` and cleaned
    up by nothing; only the SAVE slot had moved into the run's own root
    (#1616).
    """

    def __init__(self, base: str) -> None:
        self.base = base
        self.root = os.path.join(base, "root")
        self.logs = os.path.join(base, "logs")
        self.fixtures = os.path.join(base, "fixtures")

    def build(self) -> str:
        """Stage this invocation's throwaway resource root (#1616) and
        the two artifact directories beside it, and answer with the root.

        The read-only content families are symlinked; `config/` is
        COPIED without the developer's `*.local.yaml` overrides; `saves/`
        starts empty and belongs to this run.
        `app/App/ResourceRoot.hs` chdirs the engine into that directory
        and `World.Save.Serialize` resolves `saves` relative to it, so
        the round trip below writes here instead of the developer's live
        `saves/` — which is gitignored and therefore accumulates
        abandoned slots silently. Copying `config/` rather than
        symlinking it keeps a personal `config/save.local.yaml` out of
        the run: `scripts/init.lua` loads the autosave scheduler, so a
        local autosave interval could otherwise fire a competing save
        while this probe is winding the calendar around and rotate slots
        underneath it.
        """
        os.makedirs(self.root, exist_ok=True)
        for family in ("scripts", "assets", "data"):
            target = os.path.join(self.root, family)
            if not os.path.exists(target):
                os.symlink(os.path.join(REPO, family), target)
        config_dst = os.path.join(self.root, "config")
        if not os.path.exists(config_dst):
            shutil.copytree(os.path.join(REPO, "config"), config_dst,
                            ignore=shutil.ignore_patterns("*.local.yaml"))
            _make_owner_writable(config_dst)
        os.makedirs(os.path.join(self.root, "saves"), exist_ok=True)
        os.makedirs(self.logs, exist_ok=True)
        os.makedirs(self.fixtures, exist_ok=True)
        return self.root

    @property
    def engine_log(self) -> str:
        """The engine's stdout/stderr capture. `probelib.boot` opens it
        `"w"`, so this being invocation-owned is what stops a second run
        truncating a first run's evidence."""
        return os.path.join(self.logs, "engine.log")

    def fixture(self, name: str) -> str:
        """One of the probe's own inline flora fixtures. The path is
        handed to the ENGINE, which has chdir'd into `root`, so it is
        absolute for the same reason it always was."""
        return os.path.join(self.fixtures, f"{name}.yaml")


def _describe_held(path: str) -> str:
    """How the retention summary describes one artifact directory.

    A path that does not EXIST is not the same as one that is empty: a
    run that failed part-way through staging never created it, and
    calling it empty would send the reader looking for a directory that
    is not there — the same mistake as naming artifacts a failure is the
    reason the run does not have.
    """
    if not os.path.exists(path):
        return " (never created — the run ended before staging reached it)"
    try:
        held = sorted(os.listdir(path))
    except OSError as exc:
        return f" (unreadable: {exc})"
    return f" ({', '.join(held)})" if held else " (empty)"


def abandon_engine(proc) -> None:
    """Make sure an engine this run launched is dead, without talking to
    the port. The last thing tried on either teardown path, and a no-op
    on a handle that has already exited.

    Two callers, for two different reasons.

    An engine this run LAUNCHED but never took ownership of:
    `probelib.boot` hands the process over the moment it exists
    (`on_launch`) and only decides about READY up to three minutes
    later, so the handle can reach this run's teardown while `boot`
    itself is still deciding. `boot` kills the process on both of its
    OWN failure exits, so on those this finds it already gone; it exists
    for the exit `boot` never reached.

    And as the FALLBACK under an orderly shutdown, because `quit_engine`
    is itself interruptible — it sends, waits, then hard-kills, and a
    Ctrl-C in any of those unwinds straight out of the teardown with the
    engine possibly still running, while `main` goes on to delete the
    tree it is writing into. `kill()` is a single syscall and SIGKILL
    cannot be caught, so once it has landed the process is dead whatever
    happens to the reap that follows.

    Deliberately NOT `quit_engine` in either case: that sends
    `engine.quit()` to the PORT, and a boot fails on a busy port
    precisely because the port belongs to somebody else's instance. Only
    a boot that RETURNED proves this run's engine is the one listening
    there.
    """
    if proc.poll() is not None:
        return
    proc.kill()
    try:
        proc.wait(timeout=10)
    except subprocess.TimeoutExpired:
        print(f"  [FAIL] the engine this run launched (pid {proc.pid}) did "
              f"not die when killed")


def release_artifacts(art: RunArtifacts, keep: bool) -> bool:
    """Retire this invocation's artifact directory, once the engine it
    booted has been through `quit_engine`, and say whether the run may
    still report success.

    Without `--keep-artifacts` the whole tree goes away — fixtures,
    engine log and save slot together — and anything that SURVIVES makes
    the run non-zero: a green result sitting beside leftover artifacts is
    precisely the outcome this isolation exists to prevent, so it must
    not be reported as a pass. With the flag the tree is retained and
    named, and the run keeps whatever result its own checks produced.

    Only ever removes the directory THIS process made with
    `tempfile.mkdtemp`, so nothing pre-existing is at risk; `rmtree`
    unlinks the symlinked content families rather than recursing into
    them, so the real `scripts/`, `assets/` and `data/` are never
    followed.
    """
    if keep:
        # Each line names what this run ACTUALLY produced. A run that
        # died before READY holds no fixtures and no save slot, and
        # saying otherwise would send the reader looking for files the
        # failure is the reason they do not have.
        print(f"\nretained this run's artifacts (--keep-artifacts): "
              f"{art.base}")
        for label, path in (("engine log", art.logs),
                            ("fixtures", art.fixtures),
                            ("saves", os.path.join(art.root, "saves"))):
            print(f"  {label:14} {path}{_describe_held(path)}")
        print(f"  {'resource root':14} {art.root}"
              + ("" if os.path.isdir(art.root)
                 else " (never created)"))
        return True
    try:
        shutil.rmtree(art.base)
    except OSError as exc:
        print(f"  [FAIL] could not remove this run's artifact directory "
              f"{art.base}: {exc}")
        return False
    if os.path.exists(art.base):
        print(f"  [FAIL] this run's artifact directory survived removal: "
              f"{art.base}")
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


def bootstrap(port, art):
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    # The SHIPPED flora is loaded with its result read, unlike the three
    # families above (#1342 deliberately leaves shipped bulk loads
    # unasserted, and this is the documented exception to that): #1711
    # moved `regrowth_time` behind a finite/strictly-positive domain
    # check that rejects the WHOLE FILE, so a shipped file that stopped
    # registering is exactly the regression this probe has to be unable
    # to miss. `engine.loadFloraYaml` answers with the number of
    # TEXTURES it queued, not species (YamlTextures.hs folds texture
    # counts), so only the rejection signal is read here — every
    # Engine.Asset loader returns 0 and nothing else when it registered
    # nothing. The species COUNT per file is pinned in hspec
    # (Asset.FloraRegrowthSchema), where it can be read from
    # loadFloraYaml's own list.
    shipped = sorted(glob.glob("data/flora/*.yaml"))
    if not shipped:
        sys.exit("SETUP FAILURE: no data/flora/*.yaml found — the probe "
                 "cannot show that the shipped flora still registers")
    rejected = []
    for path in shipped:
        raw = send(port, f"return engine.loadFloraYaml('{path}')")
        try:
            queued = float(raw)
        except (TypeError, ValueError):
            queued = 0.0
        if queued <= 0:
            rejected.append(f"{path} (returned {raw!r})")
    if rejected:
        sys.exit("SETUP FAILURE: shipped flora rejected by the loader — "
                 + ", ".join(rejected))
    print(f"  [PASS] all {len(shipped)} shipped data/flora/*.yaml files "
          f"still register")
    # The probe's own fruiting species, registered after the shipped
    # flora simply because worldgen reads the catalog when it generates
    # a chunk. Placement is salted from each species' authored NAME
    # since #2241, so this order no longer changes anyone else's rolls;
    # what it still has to do is register before world.init.
    # Max-tolerance worldGen: places on any seed.
    berry_path = art.fixture("probe_berry")
    with open(berry_path, "w") as f:
        f.write(PROBE_BERRY_YAML)
    load_fixture_yaml(port, "engine.loadFloraYaml", berry_path)
    # The probe's own no-fruiting-stage species. Its name is distinct
    # from probe_berry's, which since #2241 is all that keeps their
    # placement rolls apart. Max-tolerance worldGen: places on any
    # seed, same as probe_berry.
    clover_path = art.fixture("probe_clover")
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


def run_probe(args, art) -> bool:
    """Stage this run's own tree, boot an engine against it, and run
    every check. Returns True when they all passed.

    Split out of `main` (#1682) so the guard around it owns exactly one
    thing — releasing this invocation's artifact directory — on every
    path out of the run, however it ends.
    """
    port = args.port
    passed = True

    root = art.build()
    # Unique per invocation as well as per root, so the slot NAME alone
    # identifies this run even in a log shared with another.
    slot = f"flora_growth_check_{uuid.uuid4().hex[:8]}"
    print(f"isolated resource root: {root}", flush=True)
    print(f"save slot: {slot}", flush=True)

    # TWO records of the same engine, because they answer two different
    # questions and the gap between them is where a live process used to
    # be stranded.
    #
    # `launched` is filled by `probelib.boot` itself, in the statement
    # after its `Popen` (#1682) — so this run holds the handle from the
    # moment the OS process exists, rather than only once `boot` returns,
    # which on a hung boot is `ready_timeout` (three minutes) later. An
    # interrupt anywhere in that span therefore still finds something to
    # dispose of.
    #
    # `proc` is set only when `boot` RETURNED, which is the separate
    # claim that this run's engine is the one now listening on `port`.
    # Only then may teardown send `engine.quit()` there: a boot fails on
    # a busy port precisely because somebody else's instance holds it,
    # and shutting THAT down is the damage this split exists to prevent.
    # The guard is opened before either, so nothing between here and the
    # `finally` can escape it.
    launched: list = []
    proc = None
    try:
        proc = boot(port, art.engine_log, args=["--resource-root", root],
                    on_launch=launched.append)
        bootstrap(port, art)
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
            return False
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

        # --- 3b. The regrowth timer actually gates the NEXT harvest ---
        # #1711: `regrowth_time` is the only thing between a harvested
        # wild plant and being harvestable again. The bare `live <= 0`
        # gate in Forage/Harvest.hs reinserts the authored value
        # unchanged, so a non-positive one is already expired the instant
        # it is written and the very next call spawns the full yield
        # again. That defect is now impossible to AUTHOR (the decoder
        # rejects it — Asset.FloraRegrowthSchema), and this is the other
        # half: with the positive duration probe_berry authors (86400
        # game-seconds), the cycle still behaves.
        #
        # Every step below runs against the SAME tile 3d just harvested,
        # so this reads that harvest's own timer rather than setting one
        # up separately.
        e3b = growth_entry(port, *rasp, "probe_berry")
        live = e3b.get("regrowthRemaining") if e3b else None
        ok3e = isinstance(live, (int, float)) and live > 0
        passed &= ok3e
        print(f"  [{'PASS' if ok3e else 'FAIL'}] the harvest started a "
              f"POSITIVE regrowth timer: {live}")
        # Immediately again, same tile, same in-season date: refused.
        y2 = send_json(port, f"return world.harvestFlora({rasp[0]},{rasp[1]})")
        ok3f = y2 is None
        passed &= ok3f
        print(f"  [{'PASS' if ok3f else 'FAIL'}] an immediate second harvest "
              f"on the same tile is refused while the timer runs: {y2}")
        # Only an actual GAME-TIME tick may reopen it — World.Thread.Time
        # decrements by dtGame (dt * scale * 60), so changing the
        # calendar date alone would prove nothing. 86400 game-seconds at
        # 3000 game-min/real-sec is ~0.5 real-seconds of ticking; 2.5s
        # leaves generous margin, and ~4 game-days keeps the raspberry
        # inside its own fruiting window (day-of-year 200 -> ~204, window
        # 180-269) so the retry is gated by the TIMER and nothing else.
        send(port, "world.setTimeScale('probe', 3000); return 'ok'")
        time.sleep(2.5)
        send(port, "world.setTimeScale('probe', 1); return 'ok'")
        e3c = growth_entry(port, *rasp, "probe_berry")
        expired = e3c.get("regrowthRemaining") if e3c else None
        ok3g = e3c is not None and expired == 0
        passed &= ok3g
        print(f"  [{'PASS' if ok3g else 'FAIL'}] the timer expired on the "
              f"game clock: {live} -> {expired}")
        y3 = send_json(port, f"return world.harvestFlora({rasp[0]},{rasp[1]})")
        ok3h = isinstance(y3, list) and len(y3) >= 1 \
            and all(item.get("id") == "wild_berries" for item in y3)
        passed &= ok3h
        print(f"  [{'PASS' if ok3h else 'FAIL'}] the same tile harvests "
              f"again once the timer expired: {y3}")

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
            return False
        send(port, "world.show('probe'); return 'ok'")
        d5 = send_json(port, "return world.getDate('probe')")
        ok5 = isinstance(d5, dict) and d5.get("year") == 3 \
            and d5.get("month") == 2 and d5.get("day") == 10
        passed &= ok5
        print(f"  [{'PASS' if ok5 else 'FAIL'}] growth clock survives "
              f"save/load: {d5}")

        return passed
    finally:
        # Orderly shutdown FIRST: the root must still exist while the
        # engine is closing its own files, and only then may this run's
        # tree — every fixture, log line and save artifact in it — be
        # released by the guard in `main`, on the failing path exactly
        # as on the passing one, and on an interrupted one too.
        if proc is not None:
            # The orderly shutdown gets its own guard, because it is
            # itself interruptible: `quit_engine` sends `engine.quit()`,
            # waits out the exit, then hard-kills, and an interrupt in
            # any of those would otherwise leave here with a live engine
            # holding the port and the log this run is about to delete.
            try:
                quit_engine(port, proc)
            finally:
                abandon_engine(proc)
        elif launched:
            abandon_engine(launched[0])


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9186)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    ap.add_argument("--keep-artifacts", action="store_true",
                    help="keep this run's artifact directory (its two "
                         "fixture YAMLs, the engine log, and the isolated "
                         "resource root with its saves) instead of deleting "
                         "it, and name it in the summary — for diagnosing a "
                         "failure")
    args = ap.parse_args()

    # This invocation owns every file it creates (#1682): the two
    # fixture YAMLs, the engine log, and the resource root whose saves/
    # the round trip at the end writes into (#1616), all under one
    # directory `tempfile.mkdtemp` gave this process alone.
    #
    # The guard starts HERE, one statement after that directory exists
    # (#1791), because everything between this point and the release
    # below can fail with invocation-owned state already on disk.
    # `RunArtifacts.build` stages incrementally — the root, three
    # symlinks, a copied `config/`, `saves/`, then the log and fixture
    # directories — so a permission, source or disk-space failure
    # part-way through leaves a partial tree that nothing outside this
    # guard would remove. `run_probe`'s `boot` is inside for the same
    # reason: it exits the probe outright when the engine dies before
    # READY, and that path would otherwise leave this run's tree sitting
    # in the temp directory.
    #
    # A `finally` rather than a set of `except` clauses, so a handled
    # Ctrl-C releases the tree on its way out and still ends the run the
    # way an interrupt should, and an unexpected exception still prints
    # its own traceback. Nothing can promise cleanup after an
    # UNCATCHABLE termination (SIGKILL, a host failure) — which is why
    # the names INSIDE the tree never have to be collision-proof on
    # their own: the tree itself already is, so even that residue cannot
    # collide with another run.
    art = RunArtifacts(tempfile.mkdtemp(prefix="synarchy_flora_growth_"))
    passed = False
    try:
        passed = run_probe(args, art)
        print("\n" + ("ALL FLORA GROWTH CHECKS PASSED" if passed
                      else "SOME FAILED"))
    except SystemExit as exc:
        # How `probelib.boot` ends a run whose engine died before READY
        # or never printed it, and how a setup step gives up. Reported
        # rather than allowed to exit, so the release below stays on the
        # path and the summary names the abort.
        print(f"\n  [FAIL] the run aborted before finishing: {exc}")
    finally:
        # Only ever after `run_probe`'s own `quit_engine`, which its
        # `finally` has already run by the time control reaches here.
        released = release_artifacts(art, args.keep_artifacts)
        if not passed and not args.keep_artifacts:
            # A failure's primary evidence is the engine log, and some
            # paths above have already named it — `probelib.boot`'s
            # abort message quotes the path verbatim. It has just been
            # deleted with the rest of the tree, so say so here rather
            # than leave the operator chasing a path that is no longer
            # there.
            print("  (this run's engine log, fixture YAMLs and save slot "
                  "went with its artifact directory — re-run with "
                  "--keep-artifacts to keep them)")
    return 0 if passed and released else 1


if __name__ == "__main__":
    try:
        sys.exit(main())
    except FixtureNotRegistered as exc:
        # Raised by `load_fixture_yaml` when one of the two fixtures
        # above registered nothing. `main`'s `finally` has already
        # released this run's artifacts on the way out; this only turns
        # the setup failure into a message rather than a traceback.
        print(f"\n{exc}")
        sys.exit(1)
