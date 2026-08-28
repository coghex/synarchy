#!/usr/bin/env python3
"""Foraging / interactive-flora probe (#94).

Boots a headless engine on a real generated world (flora placement needs
worldgen; the arena has no plants), then checks:

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

Usage: python3 tools/foraging_probe.py [--port 9173] [--seed 42]
       [--size 64] [--plates 3]
"""
import argparse, glob, os, shutil, socket, subprocess, sys
import tempfile, time, uuid
from probelib import (boot, capture_request_id, quit_engine, send, send_json,
                      wait_load_published, wait_save_complete)

SPROOT = "/tmp"
REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


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


def find_harvestable(port, span=4):
    """Scan sample points across the loaded region for the nearest
    harvestable tile; returns (gx, gy, species) or None."""
    for sx in range(-span * 16, span * 16 + 1, 32):
        for sy in range(-span * 16, span * 16 + 1, 32):
            r = send_json(port, f"return world.findHarvestableFlora({sx},{sy},64)")
            if isinstance(r, dict):
                return r["gx"], r["gy"], r["id"]
    return None


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

        proc = boot(port, f"{SPROOT}/foraging_probe_engine.log",
                    args=["--resource-root", root])
        bootstrap(port)
        send(port, f"world.init('probe', {args.seed}, {args.size}, {args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)

        # --- 1. Find + query ---
        found = find_harvestable(port)
        if not found:
            print("  [FAIL] no harvestable flora found in the loaded region "
                  "(seed/climate has no raspberry/clover here — try another seed)")
            return 1
        gx, gy, species = found
        fl = send_json(port, f"return world.getFloraAt({gx},{gy})")
        ok1 = isinstance(fl, dict) and fl.get("harvestable") is True \
              and fl.get("regrowthRemaining", -1) == 0
        passed &= ok1
        print(f"  [{'PASS' if ok1 else 'FAIL'}] getFloraAt reports harvestable: "
              f"{species} at ({gx},{gy}) → {fl}")

        # --- 2. Harvest: yields spawn, tile flips, re-harvest refused ---
        yields = send_json(port, f"return world.harvestFlora({gx},{gy})")
        ok2 = isinstance(yields, list) and len(yields) >= 1 \
              and all("gid" in y and "id" in y for y in yields)
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
        # clover 43200 gs / raspberry 86400 gs; at timeScale 3000
        # (game-min/real-sec) that's 0.24 / 0.48 real-seconds.
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
        found2 = find_harvestable(port)
        if not found2:
            print("  [FAIL] no harvestable flora on the reloaded world")
            return 1
        fgx, fgy, fspecies = found2
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
        while time.time() < deadline:
            time.sleep(2.0)
            fl5 = send_json(port, f"return world.getFloraAt({fgx},{fgy})")
            if isinstance(fl5, dict) and not fl5.get("harvestable"):
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
              f"tile_harvested={foraged} food_acquired={eaten}")

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
    sys.exit(main())
