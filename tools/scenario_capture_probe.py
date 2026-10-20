#!/usr/bin/env python3
"""Headless scenario CAPTURE probe (issue #2714, SCN-14 of epic #2698) —
the real-process evidence the GPU-free ``Scenario.Capture`` hspec group
cannot give.

The hspec group drives the production capture service against stand-in
owner threads; this probe drives it against a REAL engine (every owner
thread running) over the debug console, and crosses real process
boundaries. It uses the INTERNAL console verbs #2714 added on the ``debug``
table — ``debug.loadScenario``, ``debug.captureScenario`` and the two
``debug.getScenario*Status`` readers — which SCN-15's public entry points
supersede (they are not a public API and take a resolved path only).

Checks (exit 0 = all pass):

  1. LIVE ADMISSION. One console chunk asks for a capture and, before the
     Lua thread can take its turn, makes every competing request — an
     ordinary save, an autosave, an ordinary load, a scenario load,
     another capture and an unpause. Each is refused at once, none changes
     the capture's identity or the pause, and the capture then completes:
     the same id, paused, a complete file at the destination.
  2. EXPLICIT CURRENT VALUES. The arena is edited through the real engine
     verbs after it loaded (a stat changed, a ground item spawned); the
     capture records the CURRENT values, and the document decodes (loading
     it later) to the edited arena.
  3. OVERWRITE INTENT. A second capture to the same destination without
     overwrite is refused and changes no byte; with overwrite it replaces
     the file in one step; a different name preserves the original.
  4. ORDINARY-SAVE RESTORATION WITHOUT THE SOURCE. The arena is saved with
     the ordinary ``engine.saveWorld``, the engine is stopped, a FRESH
     process restores it with ``engine.loadSave`` (no scenario file is
     read) and captures: the document is byte-identical to the first one.
  5. FRESH-PROCESS LOAD. A THIRD fresh process loads that captured file,
     asserts the edited values, and captures it again: byte-identical.
     Nothing accumulated across the three captures.
  6. NOTHING LEFT BEHIND. The destination directory holds only the files
     the probe wrote (no temporary), and the source scenario is untouched.

Every engine runs on its own isolated resource root (real scripts, assets,
data and config symlinked, its own empty ``saves/``), on a non-8008 port.
The captured documents and the comparison are printed and, with
``--retain DIR``, copied there (committed under ``docs/pr-proofs/`` as the
evidence of record).

Run:  python3 tools/scenario_capture_probe.py [--port 9231] [--retain DIR]
"""
from __future__ import annotations

import argparse
import glob
import hashlib
import os
import shutil
import sys
import tempfile
import time
from pathlib import Path

from probelib import boot, quit_engine, send, send_json, wait_load_published

REPO = Path(__file__).resolve().parent.parent
SAVE_SLOT = "scn_capture_probe"

SCENARIO = """\
version: 3
map: {width: 40, height: 40}
camera: {x: 3, y: 4}
terrain:
  - {id: pad, rect: {x0: -3, y0: -3, x1: 3, y1: 3}, material: granite, surface_z: 6, tags: [pad]}
  - {id: pit, tiles: [[8, 8], [9, 8]], material: sand, surface_z: -2}
fluids:
  - {id: pond, rect: {x0: 8, y0: 8, x1: 9, y1: 8}, fluid: lake, surface_z: 4.375}
buildings:
  - {id: bat, definition: high_voltage_battery, x: 14, y: 4, power_charge: 2500, tags: [power]}
  - {id: crate, definition: cargo_hold_S, x: 16, y: 8, storage: [{id: bars, definition: steel_bar}]}
units:
  - id: medic
    definition: acolyte
    x: 1
    y: 1
    tags: [squad]
    stats: {hydration: 4.0, calories: 600}
    blood: 3.2
    wounds: [{part: torso, kind: slash, severity: 0.2}]
    inventory: [{id: kit, definition: field_toolbox, contents: [{id: spare, definition: steel_bar}]}]
ground_items:
  - {id: gem, definition: processing_unit, x: 10.5, y: -9.5, tags: [loot]}
"""

# the definitions the scenario names, in the order the engine's own boot
# loads them
LOADERS = [
    ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
    ("data/materials/*.yaml", "engine.loadMaterialYaml"),
    ("data/items/*.yaml", "engine.loadItemYaml"),
    ("data/equipment/*.yaml", "engine.loadEquipmentYaml"),
    ("data/factions/*.yaml", "engine.loadFactionYaml"),
    ("data/physiology/*.yaml", "engine.loadPhysiologyYaml"),
    ("data/infections/*.yaml", "engine.loadInfectionYaml"),
    ("data/units/*.yaml", "engine.loadUnitYaml"),
    ("data/buildings/*.yaml", "engine.loadBuildingYaml"),
]
SCRIPTS = [("unit_stats", 0.1), ("unit_resources", 0.2), ("unit_ai", 0.1)]

FAILED: list[str] = []


def ok(cond: bool, what: str) -> bool:
    print(("  PASS " if cond else "  FAIL ") + what)
    if not cond:
        FAILED.append(what)
    return cond


def make_isolated_root(base: str) -> str:
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def bootstrap(port: int) -> None:
    """Register the shipped definitions the scenario names."""
    for pattern, fn in LOADERS:
        for path in sorted(glob.glob(str(REPO / pattern))):
            rel = os.path.relpath(path, REPO)
            send(port, f"{fn}('{rel}'); return 'ok'")
    for script, dt in SCRIPTS:
        send(port, f"engine.loadScript('scripts/{script}.lua', {dt}); return 'ok'")


def lua_str(path: str) -> str:
    return "'" + path.replace("\\", "\\\\").replace("'", "\\'") + "'"


def wait_status(port: int, getter: str, seconds: float = 90.0):
    """Poll a ``debug.getScenario*Status`` reader until its record is terminal."""
    deadline = time.time() + seconds
    last = None
    while time.time() < deadline:
        last = send_json(port, f"return {getter}()")
        if isinstance(last, dict) and last.get("outcome"):
            return last
        time.sleep(0.2)
    return last


def load_scenario(port: int, path: str) -> dict | None:
    r = send(port, f"return debug.loadScenario({lua_str(path)})")
    if r.strip() in ("false", ""):
        sys.exit(f"FAIL: debug.loadScenario was refused: {r!r}")
    return wait_status(port, "debug.getScenarioLoadStatus")


def capture(port: int, dest: str, overwrite: bool = False) -> dict | None:
    r = send(port, f"return debug.captureScenario({lua_str(dest)}, {str(overwrite).lower()})")
    if r.strip() in ("false", ""):
        sys.exit(f"FAIL: debug.captureScenario was refused: {r!r}")
    return wait_status(port, "debug.getScenarioCaptureStatus")


def sha(path: str) -> str:
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9231)
    ap.add_argument("--retain", help="copy the captured documents and the comparison here")
    args = ap.parse_args()
    if args.port == 8008:
        sys.exit("refusing to use port 8008 (the GUI port)")

    base = tempfile.mkdtemp(prefix="scenario_capture_probe_")
    try:
        return run(args, base)
    finally:
        shutil.rmtree(base, ignore_errors=True)


def run(args, base: str) -> int:
    root = make_isolated_root(base)
    work = os.path.join(base, "work")
    os.makedirs(work)
    source = os.path.join(work, "source.yaml")
    Path(source).write_text(SCENARIO)
    source_sha = sha(source)
    first = os.path.join(work, "capture-1.yaml")
    restored = os.path.join(work, "capture-2-restored.yaml")
    final = os.path.join(work, "capture-3-fresh.yaml")
    proc = None
    port = args.port
    results: dict[str, str] = {}

    def engine(label: str):
        nonlocal proc
        proc = boot(port, log=f"/tmp/scenario_capture_probe_{port}.log", label=label,
                    args=["--resource-root", root])
        bootstrap(port)

    try:
        # ── process 1: load, edit, capture ───────────────────────────────
        print("\n--- process 1: load the scenario, edit it, capture it ---")
        engine("engine-1")
        st = load_scenario(port, source)
        ok(bool(st) and st.get("outcome") == "ready", f"the scenario loaded ready ({st})")
        ok(send(port, "return engine.isPaused()") == "true", "the loaded arena is paused")

        # edit through the real engine verbs: a stat and a new ground item
        uid = send(port, "return unit.getAllIds()[1]").strip()
        send(port, f"unit.setStat({uid}, 'hydration', 3.0); return 'ok'")
        before = send(port, f"return unit.getStat({uid}, 'hydration')").strip()
        ok(before in ("3", "3.0"), f"the live hydration was edited to 3.0 (read {before})")
        send(port, "item.spawnGround('steel_bar', 12.5, 12.5); return 'ok'")

        # 1. live admission: every competing request, made while the capture
        # is admitted and before the Lua thread (which is busy running THIS
        # chunk) can take its turn
        print("\n  live admission: competing requests while the capture is in flight")
        chunk = f"""
            local id = debug.captureScenario({lua_str(first)}, false)
            local out = {{}}
            out.capture = id
            out.save = engine.saveWorld('test_arena', 'probe_during_capture')
            out.autosave = engine.saveWorld('test_arena', 'probe_during_autosave', {{autosave = true}})
            out.load = engine.loadSave('probe_nothing')
            local lid, lwhy = debug.loadScenario({lua_str(source)})
            out.scenario_load = lid
            out.scenario_load_why = lwhy
            local cid, cwhy = debug.captureScenario({lua_str(os.path.join(work, 'second.yaml'))}, false)
            out.second_capture = cid
            out.second_capture_why = cwhy
            out.unpause = engine.setPaused(false)
            out.paused = engine.isPaused()
            return out
        """
        # the debug console reads one line per command
        racing = send_json(port, " ".join(line.strip() for line in chunk.strip().splitlines()))
        ok(isinstance(racing, dict) and isinstance(racing.get("capture"), (int, float)),
           f"the capture was accepted ({racing.get('capture') if isinstance(racing, dict) else racing})")
        for key in ("save", "autosave", "load", "scenario_load", "second_capture", "unpause"):
            ok(racing.get(key) is False, f"a competing {key.replace('_', ' ')} was refused ({racing.get(key)!r})")
        ok(racing.get("paused") is True, "the arena stayed paused through every attempt")
        capture_id = int(racing["capture"])
        done = wait_status(port, "debug.getScenarioCaptureStatus")
        ok(bool(done) and done.get("outcome") == "succeeded", f"the capture succeeded ({done})")
        ok(bool(done) and done.get("id") == capture_id, "it is the SAME request the chunk was given")
        ok(send(port, "return engine.isPaused()") == "true", "the arena is paused after the capture")
        ok(os.path.exists(first), "the destination holds the captured file")
        ok(not os.path.exists(os.path.join(work, "second.yaml")), "the refused capture wrote nothing")
        ok(sorted(os.listdir(work)) == ["capture-1.yaml", "source.yaml"],
           f"the directory holds only the file and the source ({sorted(os.listdir(work))})")
        text1 = Path(first).read_text()
        ok("hydration: 3" in text1, "the document records the CURRENT hydration (3.0), not the original 4.0")
        ok("hydration: 4" not in text1, "the original override did not come back")
        ok(text1.count("definition: steel_bar") >= 3, "the spawned ground item is in the document")
        ok(done and done.get("entries", {}).get("units") == 1, f"one unit was captured ({done and done.get('entries')})")
        results["capture-1 sha256"] = sha(first)

        # 3. overwrite intent
        print("\n  overwrite intent")
        h1 = sha(first)
        refused = capture(port, first, overwrite=False)
        ok(bool(refused) and refused.get("outcome") == "unsuccessful" and refused.get("kind") == "FailOverwriteRefused",
           f"an existing destination without overwrite is refused ({refused and refused.get('kind')})")
        ok(sha(first) == h1, "the refused capture changed none of the destination's bytes")
        ok(refused and refused.get("published") is False, "…and reports that nothing was published")
        ok(send(port, "return engine.isPaused()") == "true", "still paused after the refusal")
        replaced = capture(port, first, overwrite=True)
        ok(bool(replaced) and replaced.get("outcome") == "succeeded" and replaced.get("replaced") is True,
           f"with overwrite the file is replaced ({replaced and replaced.get('replaced')})")
        ok(sha(first) == h1, "the arena is unchanged, so the replacement is the same document")
        other = os.path.join(work, "capture-1-copy.yaml")
        saved_as = capture(port, other)
        ok(bool(saved_as) and saved_as.get("outcome") == "succeeded", "a different name (Save As) is accepted")
        ok(sha(first) == h1 and sha(other) == h1, "the original is preserved and the copy is identical")
        ok(sha(source) == source_sha, "the source scenario was never rewritten")
        ok(not any(n.startswith("scenario-capture-tmp") for n in os.listdir(work)), "no temporary file was left behind")

        # 4. ordinary save, then stop
        print("\n--- ordinary save of the arena, then stop the engine ---")
        saved = send(port, f"return engine.saveWorld('test_arena', '{SAVE_SLOT}')").strip()
        ok(saved == "true", "engine.saveWorld accepted the ordinary save")
        save_file = os.path.join(root, "saves", SAVE_SLOT, "world.synworld")
        for _ in range(150):
            if os.path.exists(save_file):
                break
            time.sleep(0.2)
        ok(os.path.exists(save_file), "the ordinary save was written")
        quit_engine(port, proc)
        proc = None

        # ── process 2: restore from the ordinary save, no scenario file ──
        print("\n--- process 2 (fresh): restore the ordinary save WITHOUT the scenario file, capture ---")
        engine("engine-2")
        r = send(port, f"return engine.loadSave('{SAVE_SLOT}')").strip()
        ok(r == "true", "engine.loadSave accepted the ordinary save")
        loaded = wait_load_published(port)
        ok(bool(loaded), f"the ordinary load published ({loaded})")
        got = capture(port, restored)
        ok(bool(got) and got.get("outcome") == "succeeded", f"the restored arena was captured ({got})")
        ok(Path(restored).read_bytes() == Path(first).read_bytes(),
           "the document captured from the RESTORED arena is byte-identical to the live capture")
        results["capture-2 sha256"] = sha(restored)
        quit_engine(port, proc)
        proc = None

        # ── process 3: load the captured file in a fresh process ─────────
        print("\n--- process 3 (fresh): load the captured file, check the values, capture again ---")
        engine("engine-3")
        st3 = load_scenario(port, restored)
        ok(bool(st3) and st3.get("outcome") == "ready", f"the captured file loaded ready ({st3})")
        uid3 = send(port, "return unit.getAllIds()[1]").strip()
        hyd = send(port, f"return unit.getStat({uid3}, 'hydration')").strip()
        ok(hyd in ("3", "3.0"), f"the fresh process has the EDITED hydration (read {hyd})")
        got3 = capture(port, final)
        ok(bool(got3) and got3.get("outcome") == "succeeded", "the fresh process captured it again")
        ok(Path(final).read_bytes() == Path(first).read_bytes(),
           "…byte-identical to the first capture: nothing accumulated across three captures")
        results["capture-3 sha256"] = sha(final)
        ok(sha(restored) == results["capture-2 sha256"] and sha(first) == results["capture-1 sha256"],
           "no capture rewrote an earlier one")
        quit_engine(port, proc)
        proc = None

        if args.retain:
            os.makedirs(args.retain, exist_ok=True)
            for name in ("source.yaml", "capture-1.yaml", "capture-2-restored.yaml", "capture-3-fresh.yaml"):
                shutil.copy(os.path.join(work, name), os.path.join(args.retain, name))
            Path(os.path.join(args.retain, "comparison.txt")).write_text(
                "\n".join(f"{k}: {v}" for k, v in sorted(results.items())) + "\n")
        print("\n--- comparison ---")
        for k, v in sorted(results.items()):
            print(f"  {k}: {v}")
    finally:
        if proc is not None:
            quit_engine(port, proc)

    print()
    if FAILED:
        print(f"FAIL: {len(FAILED)} check(s) failed")
        for f in FAILED:
            print(f"  - {f}")
        return 1
    print("PASS: every check passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
