#!/usr/bin/env python3
"""Headless movement probe.

Builds a no-generator obstacle course (scripts/movement_arena.lua) in a
headless engine, spawns a unit, commands a move across it, and samples the
unit's position / activity / pose / currentAnim over time. Prints a state
timeline and per-course pass/fail checks — so movement behaviour (pathing,
climbs, falls, ramps) can be verified WITHOUT a GPU or a human watching.

What it does:
  1. launches `--headless` on a private port,
  2. loads the substance/item/equipment/material/unit YAML defs and the AI
     scripts the loading screen would normally load (it doesn't run headless),
     plus the movement_arena Lua module,
  3. builds the chosen course on a flat arena world,
  4. spawns a unit at the course start and issues a move to the goal,
  5. polls the unit's state into a timeline,
  6. runs course-specific checks (e.g. corner_trap: the unit reaches the goal
     and does NOT freeze in the walking animation — the bug this guards).

Usage:
  python3 tools/movement_probe.py                       # corner_trap, acolyte
  python3 tools/movement_probe.py --course cliff
  python3 tools/movement_probe.py --course ramp --unit acolyte --seconds 16
  python3 tools/movement_probe.py --list                # list courses + exit

Exit code 0 = all checks for the course passed.
"""
from __future__ import annotations

import argparse
import glob
import json
import math
import socket
import subprocess
import sys
import time

LOG = "/tmp/movement_probe_engine.log"


def send(port: int, lua: str, timeout: float = 10.0) -> str:
    """Run one line of Lua in the debug console, return the result text."""
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
    """Run Lua that returns a table; parse the JSON reply (None on failure)."""
    raw = send(port, lua)
    try:
        return json.loads(raw)
    except (json.JSONDecodeError, ValueError):
        return None


def boot(port: int) -> subprocess.Popen:
    log = open(LOG, "w")
    proc = subprocess.Popen(
        ["cabal", "run", "-v0", "exe:synarchy", "--",
         "--headless", "--port", str(port)],
        stdout=log, stderr=subprocess.STDOUT,
    )
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


def bootstrap(port: int, with_resources: bool = False) -> None:
    """Load defs + AI scripts + the movement_arena module (no loading screen
    headless). With `with_resources`, also load the stat + resource ticks so
    stamina drains/regens (needed for --mode stamina)."""
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
    if with_resources:
        # Stat + resource ticks drive stamina drain/regen off the engine's
        # published activity + moveSpeed. unit_ai stays neutralised below so
        # its wander doesn't fight the commanded speeds.
        send(port, "engine.loadScript('scripts/unit_stats.lua', 0.1); return 'ok'")
        send(port, "engine.loadScript('scripts/unit_resources.lua', 0.2); return 'ok'")
    # unit_ai is auto-loaded at engine boot and its update() tick wanders
    # every unit headless — which fights the moveTo command under test. We
    # want to exercise the movement ENGINE in isolation, so neutralise the
    # AI tick (replace update with a no-op in the singleton module table the
    # engine's loadScript driver calls each frame). pcall in case the module
    # layout ever changes.
    send(port,
         "pcall(function() require('scripts.unit_ai').update = function() end end); "
         "return 'ai-off'")
    # Make the arena module available (require caches it in package.loaded).
    send(port, "require('scripts.movement_arena'); return 'ok'")


def sample(port: int, uid: int):
    """One state sample: dict {x,y,z,anim,act,pose} or None if the unit is gone."""
    lua = (
        f"local i=unit.getInfo({uid}); if not i then return 'DEAD' end; "
        f"return {{x=i.gridX,y=i.gridY,z=i.gridZ,anim=i.currentAnim,"
        f"moveSpeed=i.moveSpeed,"
        f"act=unit.getActivity({uid}),pose=unit.getPose({uid})}}"
    )
    return send_json(port, lua)


def wait_world_ready(port: int) -> bool:
    """Poll until the arena's central chunk is loaded (the build queue has
    drained). getChunkInfo returns a JSON table with `loaded`; the scalar
    surface queries return tab-separated values that aren't worth parsing
    just for readiness."""
    for _ in range(40):
        r = send_json(port, "return world.getChunkInfo(0,0)")
        if isinstance(r, dict) and r.get("loaded"):
            return True
        time.sleep(0.25)
    return False


def dist(ax, ay, bx, by) -> float:
    return math.hypot(ax - bx, ay - by)


# --- per-course validators -------------------------------------------------
# Each takes (course, samples, reached) and returns a list of (label, ok) checks.

def check_flat(course, samples, reached, moved, poses, acts):
    return [("reached the goal", reached),
            ("ended idle", samples[-1].get("act") == "idle")]


def check_corner_trap(course, samples, reached, moved, poses, acts):
    final = samples[-1]
    froze = (not reached) and final.get("act") == "walking" and not moved
    return [("reached the goal (routed around the wall)", reached),
            ("did NOT freeze in walking", not froze)]


def check_cliff(course, samples, reached, moved, poses, acts):
    climbed = "climbing" in poses or any(
        "climb" in (s.get("anim") or "") for s in samples)
    on_top = reached and samples[-1].get("z") == course["goalz"]
    return [("played a climb (Climbing pose / climb anim)", climbed),
            ("reached the top of the cliff", on_top)]


def check_fall(course, samples, reached, moved, poses, acts):
    # A fall deliberately clears the move target (startFall: "AI re-issues
    # after recovery"), so the unit can't reach the goal in one moveTo — we
    # assert the fall mechanic, not arrival. Landing outcome (collapse vs
    # death) is impact-dependent; both end on the low ground.
    fell = "falling" in poses or any(
        "fall" in (s.get("anim") or "") for s in samples)
    landed = samples[-1].get("z") == course["goalz"]
    return [("played a fall (Falling pose / fall anim)", fell),
            ("landed on the low ground (z=base)", landed)]


def check_ramp(course, samples, reached, moved, poses, acts):
    climbed = "climbing" in poses
    walked_up = reached and samples[-1].get("z") == course["goalz"]
    return [("reached the top", walked_up),
            ("WALKED up the ramp (no climb pose)", not climbed)]


VALIDATORS = {
    "flat": check_flat,
    "corner_trap": check_corner_trap,
    "cliff": check_cliff,
    "cliff1": check_cliff,
    "fall_edge": check_fall,
    "ramp": check_ramp,
    "ramp_detour": check_ramp,
    "ramp_choice": check_ramp,
}
# Expected goal z per elevation course (arena base is 0).
GOAL_Z = {"cliff": 3, "cliff1": 1, "fall_edge": 0, "ramp": 1, "ramp_detour": 1,
          "ramp_choice": 1}


def speed_of(port: int, fn: str, uid: int) -> float:
    """Query movement_speed.comfort/sprint/ordered for a unit."""
    try:
        return float(send(port,
            f"return require('scripts.movement_speed').{fn}({uid})"))
    except (ValueError, TypeError):
        return 0.0


def stamina_of(port: int, uid: int) -> float:
    try:
        return float(send(port, f"return unit.getStat({uid}, 'stamina')"))
    except (ValueError, TypeError):
        return 0.0


def run_stamina_mode(port: int, args) -> int:
    """Validate the speed/stamina model deterministically (no AI):
    a unit cruising at comfort holds stamina steady; sprinting drains it.
    Each phase uses a FRESH unit so a sprint-exhausted collapse can't
    contaminate the cruise phase."""
    bootstrap(port, with_resources=True)
    course = send_json(port,
        "return require('scripts.movement_arena').buildCourse('flat')")
    if not isinstance(course, dict):
        print("FAIL: could not build arena", file=sys.stderr)
        return 2
    if not wait_world_ready(port):
        print("FAIL: arena world never became queryable", file=sys.stderr)
        return 2

    def phase(label: str, fn: str, spawn_xy, goal_xy):
        uid = int(float(send(port,
            f"return unit.spawn('{args.unit}', {spawn_xy[0]}, {spawn_xy[1]})")))
        time.sleep(1.5)
        v = speed_of(port, fn, uid)
        send(port, f"unit.moveTo({uid}, {goal_xy[0]}, {goal_xy[1]}, {v}); return 'go'")
        start = stamina_of(port, uid)
        trail = []
        for _ in range(6):
            time.sleep(1.0)
            s = sample(port, uid)
            trail.append((stamina_of(port, uid),
                          (s or {}).get("moveSpeed", -1),
                          (s or {}).get("act", "?")))
        end = trail[-1][0]
        print(f"\n[{label}] {fn}={v:.2f}  stamina {start:.2f} -> {end:.2f}")
        for i, (st, sp, act) in enumerate(trail, 1):
            print(f"    t={i}  stamina={st:.2f}  speed={sp:.2f}  {act}")
        return uid, v, start, end, trail

    # Cruise at comfort: stamina should stay ~flat, gait should be walking.
    _, vc, c0, c1, ctrail = phase("CRUISE", "comfort", (2.5, 2.5), (2.5, 24))
    cruise_steady = abs(c1 - c0) < 1.0
    cruise_moving = any(sp > 0.05 for _, sp, _ in ctrail)

    # Sprint: stamina should drain meaningfully, gait should be running.
    _, vs, s0, s1, strail = phase("SPRINT", "sprint", (5.5, 2.5), (5.5, 24))
    sprint_drains = (s0 - s1) > 1.5
    sprint_running = any(act == "running" for _, _, act in strail)

    print("\n--- checks ---")
    checks = [
        ("comfort cruise holds stamina ~steady", cruise_steady),
        ("unit actually moved at comfort", cruise_moving),
        ("sprint drains stamina", sprint_drains),
        ("sprint plays the running gait", sprint_running),
    ]
    all_ok = True
    for label, ok in checks:
        print(f"  [{'PASS' if ok else 'FAIL'}] {label}")
        all_ok = all_ok and ok
    return 0 if all_ok else 1


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--mode", default="move", choices=["move", "stamina"],
                    help="'move' = course-based pathing/gait; "
                         "'stamina' = comfort/sprint stamina model")
    ap.add_argument("--course", default="corner_trap")
    ap.add_argument("--unit", default="acolyte")
    ap.add_argument("--speed", type=float, default=None,
                    help="override travel speed (default: the unit's "
                         "sustainable comfort speed)")
    ap.add_argument("--seconds", type=float, default=14.0)
    ap.add_argument("--port", type=int, default=9134)
    ap.add_argument("--list", action="store_true",
                    help="list available courses and exit")
    args = ap.parse_args()
    args.speed_explicit = args.speed is not None

    proc = boot(args.port)
    try:
        if args.mode == "stamina":
            return run_stamina_mode(args.port, args)

        bootstrap(args.port)

        if args.list:
            names = send_json(args.port,
                              "return require('scripts.movement_arena').listCourses()")
            print("courses:", ", ".join(names) if names else "(none)")
            return 0

        course = send_json(
            args.port,
            f"return require('scripts.movement_arena').buildCourse('{args.course}')")
        if not isinstance(course, dict) or "sx" not in course:
            print(f"FAIL: unknown/invalid course '{args.course}'", file=sys.stderr)
            return 2
        course["goalz"] = GOAL_Z.get(args.course, 0)
        sx, sy = course["sx"] + 0.5, course["sy"] + 0.5
        gx, gy = course["gx"] + 0.5, course["gy"] + 0.5
        print(f"course '{course['name']}': {course.get('note','')}")
        print(f"  start ({sx},{sy}) -> goal ({gx},{gy})")

        if not wait_world_ready(args.port):
            print("FAIL: arena world never became queryable", file=sys.stderr)
            return 2

        uid = int(float(send(args.port,
                             f"return unit.spawn('{args.unit}', {sx}, {sy})")))
        if uid < 0:
            print(f"FAIL: could not spawn '{args.unit}' (unknown def?)",
                  file=sys.stderr)
            return 2
        # Climb-mechanics courses: max out climbing skill so a random
        # mid-climb slip (a separate, legitimate mechanic) can't confound
        # the height-aware-climb / pullup checks.
        if args.course in ("cliff", "cliff1"):
            send(args.port, f"unit.setSkill({uid}, 'climbing', 100); return 'ok'")

        time.sleep(1.5)  # settle onto the ground before moving

        start = sample(args.port, uid)
        # Command travel at the unit's COMFORT speed (stamina-neutral,
        # sustainable) rather than a fixed fast value. Under the speed/
        # stamina model, driving a unit above comfort for a sustained
        # traversal drains stamina until it collapses from exhaustion —
        # realistic, but it would confound feature checks (pathing,
        # climb, fall) with collapses. The dedicated --mode stamina test
        # covers the sprint/run-gait behaviour. `--speed` still overrides
        # if a caller wants a specific pace.
        move_speed = args.speed if args.speed_explicit else speed_of(
            args.port, "comfort", uid)
        send(args.port, f"unit.moveTo({uid}, {gx}, {gy}, {move_speed}); return 'go'")

        samples: list[dict] = []
        anim_seq: list[str] = []
        steps = int(args.seconds / 0.25)
        for _ in range(steps):
            s = sample(args.port, uid)
            if isinstance(s, dict):
                samples.append(s)
                a = s.get("anim") or ""
                if not anim_seq or anim_seq[-1] != a:
                    anim_seq.append(a)
            time.sleep(0.25)

        if not samples:
            print("FAIL: unit vanished / no samples", file=sys.stderr)
            return 2

        final = samples[-1]
        reached = dist(final["x"], final["y"], gx, gy) < 1.2
        # Moved = displaced meaningfully from spawn at any point.
        moved = any(dist(s["x"], s["y"], start["x"], start["y"]) > 0.3
                    for s in samples)
        poses = {s.get("pose") for s in samples}
        acts = {s.get("act") for s in samples}

        print(f"\nanim timeline:\n  " + " -> ".join(a or "·" for a in anim_seq))
        print(f"\npose set: {sorted(p for p in poses if p)}")
        print(f"activity set: {sorted(a for a in acts if a)}")
        print(f"final: pos=({final['x']:.2f},{final['y']:.2f}) z={final['z']} "
              f"act={final['act']} pose={final['pose']} "
              f"dist_to_goal={dist(final['x'],final['y'],gx,gy):.2f}")

        validator = VALIDATORS.get(args.course, check_flat)
        checks = validator(course, samples, reached, moved, poses, acts)
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
