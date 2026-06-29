#!/usr/bin/env python3
"""Headless follow_command-priority probe (#306).

Guards the utility ladder around FOLLOW_COMMAND_UTILITY (7.0): actions
documented to outrank a player move order must actually win the AI
arbitration. The bug (#306) was that follow_command was retuned 1.0 -> 7.0
without re-deriving the actions calibrated to beat it, so a pending move
order silently suppressed an explicit pickup and a dry-canteen refill.

Unlike movement_probe.py this keeps the unit_ai tick LIVE (the arbitration
IS the thing under test) and reads the chosen action straight off the AI
state via unitAi.getState(uid).currentAction.

The target ladder (#306): dire-survival > combat/treatment > player orders
(move/pickup/dry-refill) > situational goals (find_water/notify) > work/wander.

Checks (each on a fresh acolyte on a flat arena):
  A. pickup_ground (a peer player order) beats a pending move.
  B. refill_canteen (dry) beats a pending move.
  C. a move order beats a ROUTINE goal (a fresh acolyte's find_water search).
  D. combat beats a goal: a struck goal-bound acolyte reacts with a combat
     action instead of continuing to search.

Run from the repo/worktree root (scripts/ resolve relative to CWD):
  python3 tools/follow_command_priority_probe.py [--port N] [--unit acolyte]

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import glob
import json
import socket
import subprocess
import sys
import time

LOG = "/tmp/follow_command_priority_probe.log"


def send(port: int, lua: str, timeout: float = 10.0) -> str:
    with socket.create_connection(("localhost", port), timeout=timeout) as s:
        s.sendall((lua + "\n").encode())
        chunks: list[bytes] = []
        s.settimeout(0.3)
        try:
            while True:
                b = s.recv(4096)
                if not b:
                    break
                chunks.append(b)
        except socket.timeout:
            pass
    out = b"".join(chunks).decode(errors="replace")
    results = [ln[2:].strip() for ln in out.splitlines() if ln.startswith("> ")]
    results = [r for r in results if r]
    return results[-1] if results else out.strip()


def send_json(port: int, lua: str):
    raw = send(port, lua)
    try:
        return json.loads(raw)
    except (json.JSONDecodeError, ValueError):
        return None


def boot(port: int) -> subprocess.Popen:
    log = open(LOG, "w")
    # SYNARCHY_BIN lets a Lua-only change reuse an already-built binary
    # (run with CWD = this worktree so scripts/ resolve here) instead of
    # paying a full cabal rebuild in a fresh worktree.
    import os
    binp = os.environ.get("SYNARCHY_BIN")
    cmd = ([binp] if binp else ["cabal", "run", "-v0", "exe:synarchy", "--"]) + \
        ["--headless", "--port", str(port)]
    proc = subprocess.Popen(cmd, stdout=log, stderr=subprocess.STDOUT)
    deadline = time.time() + 240
    while time.time() < deadline:
        try:
            if "READY" in open(LOG).read():
                return proc
        except FileNotFoundError:
            pass
        if proc.poll() is not None:
            sys.exit(f"engine exited before READY; see {LOG}")
        time.sleep(0.4)
    proc.kill()
    sys.exit("engine never printed READY")


def bootstrap(port: int) -> None:
    """Load the YAML defs + the stat/resource ticks the loading screen would
    normally load. unit_ai is auto-loaded at boot and we deliberately leave
    its tick RUNNING — the arbitration is what we're testing."""
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    send(port, "engine.loadScript('scripts/unit_stats.lua', 0.1); return 'ok'")
    send(port, "engine.loadScript('scripts/unit_resources.lua', 0.2); return 'ok'")
    send(port, "require('scripts.movement_arena'); return 'ok'")


def wait_world_ready(port: int) -> bool:
    for _ in range(40):
        r = send_json(port, "return world.getChunkInfo(0,0)")
        if isinstance(r, dict) and r.get("loaded"):
            return True
        time.sleep(0.25)
    return False


def current_action(port: int, uid: int) -> str:
    # The debug console serialises a returned Lua string JSON-style (with
    # surrounding quotes); strip them so callers compare bare action names.
    raw = send(port,
               f"local s=require('scripts.unit_ai').getState({uid}); "
               f"return s and s.currentAction or 'nil'")
    try:
        v = json.loads(raw)
        return v if isinstance(v, str) else raw
    except (json.JSONDecodeError, ValueError):
        return raw


def poll_for_action(port: int, uid: int, want, seconds: float = 8.0):
    """Poll currentAction; return (timeline, hit_bool). `want` is a single
    action name or a set/collection of acceptable names. Stops early on hit."""
    wants = {want} if isinstance(want, str) else set(want)
    seen: list[str] = []
    steps = int(seconds / 0.4)
    for _ in range(steps):
        a = current_action(port, uid)
        if not seen or seen[-1] != a:
            seen.append(a)
        if a in wants:
            return seen, True
        time.sleep(0.4)
    return seen, False


COMBAT_ACTIONS = {"engage", "attack_target", "retreat"}


def spawn_acolyte(port: int, unit: str, x: float, y: float) -> int:
    uid = int(float(send(port, f"return unit.spawn('{unit}', {x}, {y})")))
    time.sleep(1.5)  # settle onto ground
    return uid


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--unit", default="acolyte")
    ap.add_argument("--port", type=int, default=9163)
    ap.add_argument("--seconds", type=float, default=8.0)
    args = ap.parse_args()

    proc = boot(args.port)
    try:
        bootstrap(args.port)
        course = send_json(
            args.port,
            "return require('scripts.movement_arena').buildCourse('flat')")
        if not isinstance(course, dict) or "sx" not in course:
            print("FAIL: could not build flat arena", file=sys.stderr)
            return 2
        if not wait_world_ready(args.port):
            print("FAIL: arena world never became queryable", file=sys.stderr)
            return 2
        sx, sy = course["sx"] + 0.5, course["sy"] + 0.5
        far_x, far_y = course["gx"] + 0.5, course["gy"] + 0.5  # far move target
        print(f"flat arena: spawn ({sx},{sy}), far move target ({far_x},{far_y})")

        checks: list[tuple[str, bool]] = []

        # --- Check A: explicit pickup beats a pending move order ----------
        ua = spawn_acolyte(args.port, args.unit, sx, sy)
        if ua < 0:
            print(f"FAIL: could not spawn '{args.unit}'", file=sys.stderr)
            return 2
        # Drop a light item two tiles away.
        ix, iy = int(sx) + 2, int(sy)
        gid = send(args.port,
                   f"return item.spawnGround('granite_chunk', {ix}, {iy})")
        try:
            gid = int(float(gid))
        except (ValueError, TypeError):
            print(f"FAIL: could not spawn ground item (got {gid!r})",
                  file=sys.stderr)
            return 2
        # Move order to the far edge first, THEN an explicit pickup. Both
        # commandedTask and pickupOrder are now live; pickup (8.0) must win.
        send(args.port,
             f"require('scripts.unit_ai').commandMove({ua},{far_x},{far_y}); "
             f"return 'moved'")
        send(args.port,
             f"require('scripts.unit_ai').commandPickup({ua},{gid}); "
             f"return 'pickup'")
        seen, hit = poll_for_action(args.port, ua, "pickup_ground", args.seconds)
        print(f"\n[A pickup-beats-move] action timeline: {' -> '.join(seen)}")
        checks.append(("pickup_ground wins over a pending move order", hit))

        # --- Check B: a dry-canteen refill beats a pending move order -----
        ub = spawn_acolyte(args.port, args.unit, sx + 1, sy + 1)
        # Empty the canteen (acolytes spawn with a full 2 L canteen).
        send(args.port,
             f"unit.modifyItemFill({ub}, 'canteen_steel_2l', -5.0); return 'drained'")
        # Inject a known water source adjacent so refill has somewhere to go
        # (the FOV scanner would find it too, but injection is deterministic).
        wx, wy = int(sx) + 3, int(sy) + 3
        send(args.port,
             f"local s=require('scripts.unit_ai').getState({ub}); "
             f"s.knownWaterSources = {{{{x={wx},y={wy}}}}}; return 'water'")
        send(args.port,
             f"require('scripts.unit_ai').commandMove({ub},{far_x},{far_y}); "
             f"return 'moved'")
        seenb, hitb = poll_for_action(args.port, ub, "refill_canteen", args.seconds)
        print(f"\n[B refill-beats-move] action timeline: {' -> '.join(seenb)}")
        checks.append(("refill_canteen (dry) wins over a pending move order", hitb))

        # --- Check C: a move order beats a ROUTINE goal. A fresh acolyte
        # spawns with an active find_water goal and no known water, so absent
        # other input it searches (search_for_water). Issuing a move must pull
        # it onto follow_command -> a routine goal yields to a player order
        # (command > goals; the inversion this fix corrects).
        uc = spawn_acolyte(args.port, args.unit, sx + 2, sy + 2)
        # Confirm it is searching first (goal is live, no water known).
        pre = current_action(args.port, uc)
        send(args.port,
             f"require('scripts.unit_ai').commandMove({uc},{far_x},{far_y}); "
             f"return 'moved'")
        seenc, hitc = poll_for_action(args.port, uc, "follow_command", args.seconds)
        print(f"\n[C move-beats-goal] pre={pre}  timeline: {' -> '.join(seenc)}")
        checks.append(("follow_command beats a routine find_water goal", hitc))

        # --- Check D: combat beats a goal. Stage an attacker next to a
        # goal-bound acolyte (searching for water). When struck it must react
        # with a combat action (engage/attack_target/retreat), NOT keep
        # searching -- self-defense outranks a routine goal.
        ud = spawn_acolyte(args.port, args.unit, sx + 4, sy)
        atk = spawn_acolyte(args.port, args.unit, sx + 5, sy)  # adjacent
        send(args.port,
             f"require('scripts.unit_ai').commandAttack({atk},{ud}); return 'fight'")
        seend, hitd = poll_for_action(args.port, ud, COMBAT_ACTIONS, args.seconds + 6)
        print(f"\n[D combat-beats-goal] victim timeline: {' -> '.join(seend)}")
        checks.append(("a struck goal-bound unit reacts with combat, not search",
                       hitd))

        print("\n--- checks ---")
        all_ok = True
        for label, ok in checks:
            print(f"  [{'PASS' if ok else 'FAIL'}] {label}")
            all_ok = all_ok and ok
        return 0 if all_ok else 1
    finally:
        try:
            send(args.port, "engine.quit()", timeout=2)
        except OSError:
            pass
        time.sleep(1)
        if proc.poll() is None:
            proc.kill()


if __name__ == "__main__":
    sys.exit(main())
