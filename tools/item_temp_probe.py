#!/usr/bin/env python3
"""Item temperature probe (#344).

Boots a headless engine on a real generated world and checks the
iiTemp / cooling-tick stack end-to-end:

  1. Ambient default: a bare-spawned ground item reads the tile's
     ambient (world.getAmbientAt) through item.getGroundTemp.
  2. Cooling: an item spawned hot ({temp=100}) cools monotonically
     toward ambient on the game clock; a cold one (-40) warms.
  3. Newtonian rate: of two items on the same tile, both placed on
     one side of the ambient the probe measured, the one further from
     ambient closes more °C in the same interval.
  4. Pause: the pause flag freezes cooling (same gate as flora
     regrowth).
  5. Held items: unit.setItemTemp / unit.getItemTemp round-trip on a
     carried item, the tracked temp surfaces in unit.getInventory, and
     the item cools in a unit's inventory too.
  6. Save/load: a tracked temperature survives save → loadSave (v68).
     The engine runs on a throwaway resource root, so that slot lands
     in this run's own saves/ and is deleted with it — the developer's
     saves/ is never read, written or rotated (#1613).

Usage: python3 tools/item_temp_probe.py [--port 9177] [--seed 42]
       [--size 64] [--plates 3]
"""
import argparse, glob, math, os, shutil, socket, subprocess, sys
import tempfile, time, uuid
from probelib import (boot, capture_request_id, quit_engine, send,
                      wait_load_published, wait_save_complete)

SPROOT = "/tmp"
REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# Phase 3's two items are placed relative to the ambient this run
# actually measures instead of at fixed temperatures (#1611). The old
# fixed 100/60 pair only carried a valid relation below a hard-coded
# 45°C ambient, so the comparison was skipped -- and the probe still
# reported success -- across every warmer world. These offsets reproduce
# the gaps that pair had at this fixture's own ambient (36.0°C), so the
# default run measures exactly what it measured before while every other
# ambient now gets the same check rather than none.
RATE_FAR_OFFSET = 64.0
RATE_NEAR_OFFSET = 24.0
# What the pair the engine actually stored has to look like for the
# comparison to be about behaviour rather than sampling resolution. An
# item within `Item.Temperature.itemTempSnapEpsilon` of ambient is
# untracked and reads exactly ambient, reporting its whole initial gap as
# a closure, so both items keep a healthy margin above that; the
# separation is what makes "further from ambient" a real difference.
TEMP_SNAP_EPSILON = 0.25
RATE_MIN_GAP = 4.0
RATE_MIN_SEPARATION = 8.0


def make_isolated_root(base: str) -> str:
    """A throwaway resource root for one invocation (#1613): the
    read-only content families symlinked, `config/` COPIED without the
    developer's `*.local.yaml` overrides, and its OWN empty `saves/`.

    `app/App/ResourceRoot.hs` chdirs the engine into this directory and
    `World.Save.Serialize` resolves `saves` relative to it, so the round
    trip in phase 6 writes here instead of the developer's live
    `saves/` — which is gitignored and therefore accumulates abandoned
    slots silently. Copying `config/` rather than symlinking it keeps a
    personal `config/save.local.yaml` out of the run: `scripts/init.lua`
    loads the autosave scheduler, so a local autosave interval could
    otherwise fire a competing save mid-probe and rotate slots
    underneath it.

    Only the SAVE slot moves here; `bootstrap` still feeds the engine
    repo-relative `data/**.yaml` paths, which resolve through the
    symlinked `data/` exactly as before.
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
    `tempfile.mkdtemp`, so nothing pre-existing is at risk — in
    particular the abandoned `saves/item_temp_v68_check/` already in the
    developer's checkout is neither read nor touched. `rmtree` unlinks
    the symlinked content families rather than recursing into them, so
    the real `scripts/`, `assets/` and `data/` are never followed. A
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
    requests (#1613).

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


def num(port, lua, timeout=10.0):
    raw = send(port, lua, timeout).strip('"')
    try:
        return float(raw)
    except ValueError:
        return None


def finite(value):
    """Is this reading usable in temperature arithmetic?

    `num` answers None for a non-numeric reply and hands `nan`/`inf`
    straight through, while `iiTemp` is an engine `Float`. Phase 3 places
    its fixture relative to a measured ambient, so every value it
    computes with is put through this first: a comparison against a NaN
    is silently false rather than an error, which is the shape of failure
    this probe exists to not report as success.
    """
    return isinstance(value, float) and math.isfinite(value)


def deg(value):
    """A temperature for a diagnostic line: two decimals across every
    scale a world ambient plausibly reaches, compact scientific notation
    beyond it — so a pathological fixture reports `1e+30` rather than
    thirty-one digits of the same thing."""
    return f"{value:.2f}" if abs(value) < 1e6 else f"{value:.6g}"


def ground_temp(port, gid):
    """Read one ground item's temperature, tolerating an item that never
    spawned: None in, None out, rather than a query built around a
    missing id."""
    if gid is None:
        return None
    return num(port, f"return item.getGroundTemp({gid})")


def spawn_rate_item(port, temp):
    """Spawn one of phase 3's ambient-derived items and read back what
    the engine actually stored.

    Returns `(gid, observed)`, either element None when the step it names
    produced no usable number: `item.spawnGround` answers nil on a
    rejected spawn and `item.getGroundTemp` on an unknown id. The read is
    not a formality -- `item.spawnGround` narrows the requested Lua
    number to a `Float` -- so the stored value is the only one phase 3
    judges its fixture on. Ground-item ids are zero-based, so 0 is a real
    id.
    """
    gid = num(port, "return item.spawnGround('steel_bar', 2.5, 2.5, "
                    f"{{temp={temp:.4f}}})")
    if not finite(gid) or gid < 0:
        return None, None
    gid = int(gid)
    return gid, ground_temp(port, gid)


def rate_fixture_failure(amb, near_want, far_want,
                         gid_near, t0_near, gid_far, t0_far):
    """Say why phase 3's fixture cannot carry the rate comparison, or
    None when it can.

    The probe never skips this phase (#1611), so an ambient that genuinely
    cannot support a valid pair has to surface as a failure naming the
    ambient, the temperatures the probe wanted, and what was wrong with
    them. The judgement is made on what the engine STORED rather than on
    the Python floats that were asked for: the spawn narrows to `Float`,
    so at an extreme ambient two distinct requests can collapse onto each
    other, or onto ambient itself.
    """
    wanted = (f"ambient {deg(amb)}°C, wanted {deg(near_want)}°C and "
              f"{deg(far_want)}°C")
    if gid_near is None or gid_far is None:
        return (f"{wanted}: the engine would not spawn the pair "
                f"(ids {gid_near}, {gid_far})")
    if not finite(t0_near) or not finite(t0_far):
        return (f"{wanted}: the engine read back unusable initial "
                f"temperatures ({t0_near!r}, {t0_far!r})")
    stored = f"the stored pair {deg(t0_near)}°C / {deg(t0_far)}°C"
    gap_near = t0_near - amb
    gap_far = t0_far - amb
    if gap_near <= 0 or gap_far <= 0:
        return (f"{wanted}: {stored} is not both above ambient, so the two "
                f"items would not close their gaps in one direction")
    if min(gap_near, gap_far) < RATE_MIN_GAP:
        return (f"{wanted}: {stored} leaves only "
                f"{deg(min(gap_near, gap_far))}°C above ambient, under the "
                f"{deg(RATE_MIN_GAP)}°C this comparison needs to stay clear "
                f"of the {deg(TEMP_SNAP_EPSILON)}°C snap-to-ambient "
                f"threshold")
    if gap_far - gap_near < RATE_MIN_SEPARATION:
        return (f"{wanted}: {stored} sits {deg(gap_far - gap_near)}°C apart, "
                f"under the {deg(RATE_MIN_SEPARATION)}°C separation that "
                f"makes one item measurably further from ambient")
    return None


def bootstrap(port):
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9177)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    args = ap.parse_args()
    port = args.port
    passed = True

    # This invocation owns its resource root and therefore its saves/
    # (#1613): phase 6 writes a slot that no ordinary `cabal run` can
    # reach, and the whole tree goes away below.
    base = tempfile.mkdtemp(prefix="synarchy_item_temp_")

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
    try:
        root = make_isolated_root(base)
        # Unique per invocation as well as per root, so the slot NAME alone
        # identifies this run even in a log shared with another.
        slot = f"item_temp_v68_check_{uuid.uuid4().hex[:8]}"
        print(f"isolated resource root: {root}", flush=True)
        print(f"save slot: {slot}", flush=True)

        proc = boot(port, f"{SPROOT}/item_temp_probe_engine.log",
                    args=["--resource-root", root])
        bootstrap(port)
        send(port, f"world.init('probe', {args.seed}, {args.size}, "
                   f"{args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-2, -2, 2, 2)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)

        # --- 1. Bare spawn reads the tile's ambient ---
        amb = num(port, "return world.getAmbientAt(2, 2)")
        gid_plain = int(num(port,
            "return item.spawnGround('steel_bar', 2.5, 2.5)"))
        t_plain = num(port, f"return item.getGroundTemp({gid_plain})")
        ok1 = amb is not None and t_plain is not None \
              and abs(t_plain - amb) < 0.01
        passed &= ok1
        print(f"  [{'PASS' if ok1 else 'FAIL'}] bare spawn reads ambient: "
              f"getGroundTemp={t_plain} ambient={amb}")

        # Phase 3 places its fixture relative to THIS reading, so an
        # unusable ambient is a fixture failure to report rather than a
        # value to compute with. Stopping here is what keeps requirement
        # 4's diagnostic a diagnostic: every later phase compares against
        # `amb` too, and a None would raise out of phase 2 before the
        # rate fixture ever got to explain itself.
        if not finite(amb):
            print(f"  [FAIL] the ambient at (2, 2) is unusable ({amb!r}); "
                  f"the rate fixture places its two items at "
                  f"ambient+{RATE_NEAR_OFFSET:.0f}°C and "
                  f"ambient+{RATE_FAR_OFFSET:.0f}°C and can locate neither")
            return 1

        # --- 2. Hot cools / cold warms on the game clock ---
        gid_hot = int(num(port,
            "return item.spawnGround('steel_bar', 2.5, 2.5, {temp=100})"))
        gid_cold = int(num(port,
            "return item.spawnGround('steel_bar', 2.5, 2.5, {temp=-40})"))
        t0_hot = num(port, f"return item.getGroundTemp({gid_hot})")

        # Phase 3's own pair, on the same tile and the same clock but
        # deliberately NOT phase 2's fixed items: those two anchor the
        # monotonic direction checks at 100 and -40 and the pause gate
        # below, and they stay exactly where they are.
        near_want = amb + RATE_NEAR_OFFSET
        far_want = amb + RATE_FAR_OFFSET
        gid_near, t0_near = spawn_rate_item(port, near_want)
        gid_far, t0_far = spawn_rate_item(port, far_want)
        rate_setup = rate_fixture_failure(amb, near_want, far_want,
                                          gid_near, t0_near, gid_far, t0_far)

        # steel_bar = 0.5 kg → tau = 1800 game-sec; timeScale 10 ticks
        # 600 game-sec per real-second, so ~1 tau every 3 s of polling.
        send(port, "world.setTimeScale('probe', 10); return 'ok'")
        hot_series = [t0_hot]
        for _ in range(6):
            time.sleep(1.0)
            hot_series.append(num(port, f"return item.getGroundTemp({gid_hot})"))
        t1_cold = num(port, f"return item.getGroundTemp({gid_cold})")
        # Read back to back and immediately after the loop, so the two
        # items really did have the same interval to close their gaps in.
        t1_near = ground_temp(port, gid_near)
        t1_far = ground_temp(port, gid_far)
        ok2 = all(b < a for a, b in zip(hot_series, hot_series[1:])) \
              and hot_series[-1] > amb
        ok2b = t1_cold > -40 and t1_cold < amb
        passed &= ok2 and ok2b
        print(f"  [{'PASS' if ok2 else 'FAIL'}] hot item cools monotonically "
              f"toward ambient: {[round(t, 1) for t in hot_series]}")
        print(f"  [{'PASS' if ok2b else 'FAIL'}] cold item warms toward "
              f"ambient: -40 → {t1_cold}")

        # --- 3. Newtonian rate: bigger ΔT closes more °C ---
        # Asserted at every ambient (#1611): the pair was derived from the
        # ambient measured above, so there is no band in which this phase
        # reports success without comparing the two closures.
        if rate_setup:
            ok3 = False
            print(f"  [FAIL] rate comparison has no usable fixture: "
                  f"{rate_setup}")
        elif not finite(t1_near) or not finite(t1_far):
            ok3 = False
            print(f"  [FAIL] rate comparison could not read both items back "
                  f"after the interval ({t1_near!r}, {t1_far!r}) — started "
                  f"at {deg(t0_near)}°C and {deg(t0_far)}°C over ambient "
                  f"{deg(amb)}°C")
        else:
            drop_near = t0_near - t1_near
            drop_far = t0_far - t1_far
            ok3 = drop_far > drop_near > 0
            print(f"  [{'PASS' if ok3 else 'FAIL'}] hotter item sheds more "
                  f"°C in the same time: over ambient {amb:.1f}°C, "
                  f"{t0_far:.1f}°C shed {drop_far:.1f} vs {t0_near:.1f}°C "
                  f"shed {drop_near:.1f}")
        passed &= ok3

        # --- 4. Pause freezes cooling ---
        send(port, "engine.setPaused(true); return 'ok'")
        p0 = num(port, f"return item.getGroundTemp({gid_hot})")
        time.sleep(2.0)
        p1 = num(port, f"return item.getGroundTemp({gid_hot})")
        ok4 = p0 is not None and p0 == p1
        passed &= ok4
        print(f"  [{'PASS' if ok4 else 'FAIL'}] pause freezes cooling: "
              f"{p0} == {p1}")
        send(port, "engine.setPaused(false); return 'ok'")

        # --- 5. Held item: set / get / getInventory / cools ---
        send(port, "world.setTimeScale('probe', 10); return 'ok'")
        uid = int(num(port, "local u=unit.spawn('acolyte', 2, 2); return u"))
        if uid < 0:
            print("  [FAIL] could not spawn unit")
            return 1
        time.sleep(1.0)
        send(port, f"unit.addItem({uid}, 'steel_bar'); return 'ok'")
        iid = int(num(port,
            f"local inv=unit.getInventory({uid}); "
            f"for _,it in ipairs(inv) do "
            f"if it.defName=='steel_bar' then return it.instanceId end end; "
            f"return -1"))
        amb_u = num(port, "return world.getAmbientAt(2, 2)")
        t_held0 = num(port, f"return unit.getItemTemp({uid}, {iid})")
        ok5a = iid > 0 and t_held0 is not None \
               and abs(t_held0 - amb_u) < 0.01
        send(port, f"unit.setItemTemp({uid}, {iid}, 90); return 'ok'")
        row_t = num(port,
            f"local inv=unit.getInventory({uid}); "
            f"for _,it in ipairs(inv) do "
            f"if it.instanceId=={iid} then return it.temp or -999 end end; "
            f"return -999")
        ok5b = row_t is not None and 80 <= row_t <= 90
        time.sleep(4.0)
        t_held1 = num(port, f"return unit.getItemTemp({uid}, {iid})")
        ok5c = t_held1 is not None and amb_u < t_held1 < 88
        passed &= ok5a and ok5b and ok5c
        print(f"  [{'PASS' if ok5a else 'FAIL'}] untracked held item reads "
              f"holder-tile ambient: {t_held0} vs {amb_u}")
        print(f"  [{'PASS' if ok5b else 'FAIL'}] setItemTemp surfaces in "
              f"getInventory row: temp={row_t}")
        print(f"  [{'PASS' if ok5c else 'FAIL'}] held item cools in "
              f"inventory: 90 → {t_held1}")

        # --- 6. Tracked temp survives save/load ---
        send(port, "engine.setPaused(true); return 'ok'")
        gid_save = int(num(port,
            "return item.spawnGround('steel_bar', 3.5, 3.5, {temp=100})"))
        pre = num(port, f"return item.getGroundTemp({gid_save})")
        failure = save_and_reload(port, "probe", slot)
        if failure:
            print(f"  [FAIL] {failure}")
            return 1
        send(port, "world.show('probe'); return 'ok'")
        post = num(port, f"return item.getGroundTemp({gid_save})")
        # Loaded worlds come up paused, so the tracked value should be
        # exactly what the pre-save (paused) read saw.
        ok6 = pre is not None and post is not None and abs(post - pre) < 0.5
        passed &= ok6
        print(f"  [{'PASS' if ok6 else 'FAIL'}] tracked temp survives "
              f"save/load: {pre} → {post}")

        print("\n" + ("ALL ITEM-TEMP CHECKS PASSED" if passed
                      else "SOME FAILED"))
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
    sys.exit(main())
