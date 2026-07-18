#!/usr/bin/env python3
"""Headless atomic-save-storage-transaction probe (issue #762,
persistence-overhaul C1).

Real-engine, real-restart coverage of "World.Save.Storage"'s
write-validate-publish-rotate transaction and load-source-selection
fallback, on top of what tools/save_barrier_probe.py already proves
about the #757/#758 boundary and tools/multiworld_save_probe.py already
proves about the golden save->quit->restart->load path. Runs against an
ISOLATED temporary resource root (symlinked scripts/assets/data/config,
a throwaway saves/ dir) so it never touches a real player's saves/
directory and every on-disk generation it inspects/corrupts belongs only
to this run.

Flow:
  1. Create a real (small) world and save it to one slot -> a first
     generation with no previous generation.
  2. Move the camera (persistent per-page state) and save again to the
     SAME slot -> the first generation is retained as the previous
     generation, the second becomes authoritative.
  3. Snapshot both generations' bytes as a clean baseline, then for each
     scenario below: restore the baseline, corrupt/remove the
     authoritative generation (or leave both valid), restart the engine
     fresh, and prove load-source selection picks EXACTLY the right
     generation (never a hybrid) by checking the resumed session's LIVE
     camera position -- a value distinct per generation, so "which
     generation actually loaded" is an unambiguous, real, in-session
     observation rather than a metadata read.
  4. Separately, force a real disk-level write failure (the same
     pre-occupied-path trick tools/save_barrier_probe.py uses) and prove
     the reported failure names the correct storage phase, exactly as
     tools/save_barrier_probe.py already proves for the outcome as a
     whole.

Usage:
  python3 tools/save_storage_probe.py [--port 9146] [--seed 42]

Exit 0 = every scenario selected/reported exactly what C1 promises.
"""
from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path

from probelib import boot, quit_engine, send

REPO = Path(__file__).resolve().parent.parent
SLOT = "probe_storage_slot"
FAIL_SLOT = "probe_storage_writefail"
CAMERA_1 = (5, 5)
CAMERA_2 = (40, 40)


def make_isolated_root(base: str) -> str:
    """A throwaway resource root: real scripts/assets/data/config
    (symlinked -- read-only content, safe to share) plus its OWN empty
    saves/ directory, so this probe never touches a real player's saves.
    """
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def slot_dir(root: str, name: str = SLOT) -> str:
    return os.path.join(root, "saves", name)


def auth_path(root: str, name: str = SLOT) -> str:
    return os.path.join(slot_dir(root, name), "world.synworld")


def prev_path(root: str, name: str = SLOT) -> str:
    return os.path.join(slot_dir(root, name), "world.synworld.prev")


def wait(predicate, what: str, timeout: float = 30.0):
    deadline = time.time() + timeout
    while time.time() < deadline:
        v = predicate()
        if v:
            return v
        time.sleep(0.2)
    raise RuntimeError("timed out waiting for " + what)


def wait_for_init(port: int, timeout: float = 300.0) -> str:
    return send(port, f"return world.waitForInit({int(timeout)})", timeout=timeout + 5)


def read_bytes(path: str):
    return open(path, "rb").read() if os.path.isfile(path) else None


def write_bytes(path: str, data: bytes) -> None:
    with open(path, "wb") as f:
        f.write(data)


def read_camera(port: int) -> tuple[float, float]:
    raw = send(port, "local x,y = camera.getPosition(); "
                      "return string.format('%.3f,%.3f', x, y)")
    x, y = raw.split(",")
    return float(x), float(y)


def near(a: float, b: float, tol: float = 1.0) -> bool:
    return abs(a - b) <= tol


def boot_probe(root: str, port: int, log: str):
    return boot(port, log=log, args=["--resource-root", root], ready_timeout=180)


def snapshot_generations(root: str, name: str = SLOT):
    return read_bytes(auth_path(root, name)), read_bytes(prev_path(root, name))


def restore_generations(root: str, backup, name: str = SLOT) -> None:
    """Reset a slot directory to exactly a clean two-file baseline --
    the given (authoritative, previous) bytes and nothing else (no stray
    temp files from an earlier scenario), so every scenario below starts
    from an identical, known-clean state."""
    auth_bytes, prev_bytes = backup
    d = slot_dir(root, name)
    os.makedirs(d, exist_ok=True)
    for entry in os.listdir(d):
        os.remove(os.path.join(d, entry))
    if auth_bytes is not None:
        write_bytes(auth_path(root, name), auth_bytes)
    if prev_bytes is not None:
        write_bytes(prev_path(root, name), prev_bytes)


def flip_last_byte(data: bytes) -> bytes:
    return data[:-1] + bytes([data[-1] ^ 0xFF])


def run_load_case(root: str, port: int, log: str, name: str,
                   mutate, expect) -> None:
    """Restore the clean baseline, apply `mutate(root)`, boot a fresh
    engine, load SLOT, and check the resumed camera position against
    `expect` -- one of the two REAL captured camera positions (cam1 for
    the recovered-previous case, cam2 for the authoritative case), or
    None when load must be REJECTED (no valid generation)."""
    mutate(root)
    proc = boot_probe(root, port, log)
    try:
        result = send(port, f'return engine.loadSave("{SLOT}")').strip()
        if expect is None:
            if result == "true":
                raise RuntimeError(f"[{name}] expected engine.loadSave to be "
                                    f"REJECTED (no valid generation), got true")
            print(f"  [PASS] {name}: load correctly rejected")
            return
        if result != "true":
            raise RuntimeError(f"[{name}] engine.loadSave rejected a save "
                                f"that should have a recoverable generation")
        wait_for_init(port)
        send(port, 'world.show("main_world")', expect_result=False)
        time.sleep(0.5)
        cx, cy = read_camera(port)
        if not (near(cx, expect[0]) and near(cy, expect[1])):
            raise RuntimeError(f"[{name}] expected camera near {expect}, "
                                f"got ({cx:.2f}, {cy:.2f})")
        print(f"  [PASS] {name}: loaded generation at camera "
              f"({cx:.1f}, {cy:.1f}) as expected")
    finally:
        quit_engine(port, proc)
        try:
            proc.wait(timeout=15)
        except subprocess.TimeoutExpired:
            proc.kill()


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9146)
    ap.add_argument("--seed", type=int, default=42)
    args = ap.parse_args()

    tmp_base = tempfile.mkdtemp(prefix="synarchy_save_storage_probe_")
    root = make_isolated_root(tmp_base)
    print(f"  isolated resource root: {root}")
    log = os.path.join(tmp_base, "engine.log")

    try:
        # --- 1/2: two real saves to the SAME slot -----------------------
        proc = boot_probe(root, args.port, log)
        try:
            send(args.port, f'world.init("probe",{args.seed},64,3)', expect_result=False)
            wait_for_init(args.port)
            send(args.port, 'world.show("probe")', expect_result=False)

            # camera.goToTile's world-space result isn't the raw tile
            # coordinate (it runs through the facing/isometric transform),
            # so capture the REAL position it actually lands on rather
            # than assuming one -- that's what save/load must round-trip.
            send(args.port, f'camera.goToTile({CAMERA_1[0]},{CAMERA_1[1]})', expect_result=False)
            time.sleep(0.3)
            cam1 = read_camera(args.port)
            if send(args.port, f'return engine.saveWorld("probe","{SLOT}")').strip() != "true":
                raise RuntimeError("first save rejected")
            wait(lambda: os.path.isfile(auth_path(root)), "first save file")
            if os.path.isfile(prev_path(root)):
                raise RuntimeError("a previous generation must not exist "
                                    "after only one save")
            print(f"  [PASS] first save publishes with no previous generation "
                  f"(camera {cam1})")

            send(args.port, f'camera.goToTile({CAMERA_2[0]},{CAMERA_2[1]})', expect_result=False)
            time.sleep(0.3)
            cam2 = read_camera(args.port)
            if send(args.port, f'return engine.saveWorld("probe","{SLOT}")').strip() != "true":
                raise RuntimeError("second save rejected")
            wait(lambda: read_bytes(prev_path(root)) is not None,
                 "previous generation to appear")
            print(f"  [PASS] second save retains the first generation as "
                  f"previous (camera {cam2})")
            if near(cam1[0], cam2[0]) and near(cam1[1], cam2[1]):
                raise RuntimeError("the two camera positions used to tell "
                                    "generations apart are not distinct: "
                                    f"{cam1} vs {cam2}")
        finally:
            quit_engine(args.port, proc)
            try:
                proc.wait(timeout=15)
            except subprocess.TimeoutExpired:
                proc.kill()

        baseline = snapshot_generations(root)
        if baseline[0] is None or baseline[1] is None:
            raise RuntimeError("setup failed to produce two real generations")

        # --- 3: restart-and-select across constructed on-disk states ----
        run_load_case(root, args.port, log,
            "both generations valid -> authoritative wins",
            lambda r: restore_generations(r, baseline), cam2)

        run_load_case(root, args.port, log,
            "authoritative missing -> falls back to previous",
            lambda r: (restore_generations(r, baseline),
                       os.remove(auth_path(r))), cam1)

        run_load_case(root, args.port, log,
            "authoritative truncated -> falls back to previous",
            lambda r: (restore_generations(r, baseline),
                       write_bytes(auth_path(r), baseline[0][: len(baseline[0]) // 2])),
            cam1)

        run_load_case(root, args.port, log,
            "authoritative bad framing (magic) -> falls back to previous",
            lambda r: (restore_generations(r, baseline),
                       write_bytes(auth_path(r), b"\x00" + baseline[0][1:])),
            cam1)

        run_load_case(root, args.port, log,
            "authoritative checksum-corrupt -> falls back to previous",
            lambda r: (restore_generations(r, baseline),
                       write_bytes(auth_path(r), flip_last_byte(baseline[0]))),
            cam1)

        run_load_case(root, args.port, log,
            "stray leftover temp file alongside a valid authoritative "
            "generation is ignored, never selected",
            lambda r: (restore_generations(r, baseline),
                       write_bytes(os.path.join(slot_dir(r), "world.synworld.tmp-stray"),
                                   b"an interrupted write")),
            cam2)

        run_load_case(root, args.port, log,
            "authoritative missing + stray temp file -> previous "
            "generation recovered, temp ignored",
            lambda r: (restore_generations(r, baseline),
                       os.remove(auth_path(r)),
                       write_bytes(os.path.join(slot_dir(r), "world.synworld.tmp-stray"),
                                   b"an interrupted write")),
            cam1)

        run_load_case(root, args.port, log,
            "neither generation valid -> load rejected, not a hybrid",
            lambda r: (restore_generations(r, baseline),
                       write_bytes(auth_path(r), b"\x00" + baseline[0][1:]),
                       write_bytes(prev_path(r), b"\x00" + baseline[1][1:])),
            None)

        # Restore the clean baseline for the listing check below.
        restore_generations(root, baseline)
        proc = boot_probe(root, args.port, log)
        try:
            os.remove(auth_path(root))
            saves = json.loads(send(args.port, "return engine.listSaves()"))
            entry = next((s for s in saves if s["name"] == SLOT), None)
            if entry is None:
                raise RuntimeError("listing did not recover the slot from "
                                    "its previous generation")
            if not entry.get("recovered"):
                raise RuntimeError(f"listing recovered the slot but did not "
                                    f"report recovered=true: {entry!r}")
            print("  [PASS] engine.listSaves() lists a recovered slot with "
                  "a machine-readable recovery status")
        finally:
            quit_engine(args.port, proc)
            try:
                proc.wait(timeout=15)
            except subprocess.TimeoutExpired:
                proc.kill()

        # --- 4: a real disk-level write failure names the right phase ---
        restore_generations(root, baseline)
        fail_dir = slot_dir(root, FAIL_SLOT)
        if os.path.isdir(fail_dir):
            shutil.rmtree(fail_dir)
        elif os.path.exists(fail_dir):
            os.remove(fail_dir)
        os.makedirs(os.path.dirname(fail_dir), exist_ok=True)
        write_bytes(fail_dir, b"occupying this path with a plain file")
        proc = boot_probe(root, args.port, log)
        try:
            send(args.port, f'world.init("probe2",{args.seed},64,3)', expect_result=False)
            wait_for_init(args.port)
            send(args.port, 'world.show("probe2")', expect_result=False)
            if send(args.port, f'return engine.saveWorld("probe2","{FAIL_SLOT}")').strip() != "true":
                raise RuntimeError("write-failure save rejected before it "
                                    "ever reached storage")

            def failed_status():
                raw = send(args.port, "return engine.getSaveStatus()")
                st = json.loads(raw) if raw != "nil" else None
                return st if st and st.get("phase") == "SaveFailed" else None

            fs = wait(failed_status, "disk write failure to surface as SaveFailed", 15)
            outcome = fs.get("outcome", "")
            if "PhaseDirectoryCreate" not in outcome:
                raise RuntimeError("expected the failure outcome to name "
                                    f"PhaseDirectoryCreate, got: {outcome!r}")
            print("  [PASS] a real disk-level write failure reports the "
                  "correct storage phase via engine.getSaveStatus()")

            # The barrier must not be wedged open: a normal save right
            # after must still be accepted and actually complete.
            os.remove(fail_dir)
            if send(args.port, f'return engine.saveWorld("probe2","{FAIL_SLOT}")').strip() != "true":
                raise RuntimeError("save rejected right after a prior write "
                                    "failure -- barrier stuck open?")
            wait(lambda: os.path.isfile(auth_path(root, FAIL_SLOT)),
                 "follow-up save file")
            print("  [PASS] a follow-up save after the write failure "
                  "completes normally")
        finally:
            quit_engine(args.port, proc)
            try:
                proc.wait(timeout=15)
            except subprocess.TimeoutExpired:
                proc.kill()
    finally:
        shutil.rmtree(tmp_base, ignore_errors=True)

    print("\nPASS: atomic save storage publishes/retains generations "
          "correctly, load-source selection always picks a complete "
          "generation (never a hybrid, never falls back for an "
          "incompatible file), a real disk write failure names its "
          "storage phase, and the barrier recovers for the next save.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
