#!/usr/bin/env python3
"""Homeostasis / physiology regression probe.

Spawns a batch of acolytes into a series of controlled ENVIRONMENTS and checks
they behave sanely — the safety net for the growing thermoregulation /
circulation / salt / (future) heart-rate / blood-oxygen / brain systems.

The environments are driven by thermo.debugAmbient / debugHumidity (the flat
arena's own climate is a fixed ~0°C), so each scenario is fast, controlled, and
repeatable — no world-gen needed. Add real generated-world scenarios later if
wanted.

What it guards (the point):
  * TEMPERATE → everyone stays fine. The key regression guard: if a future
    physiology change makes units freeze/cook/cramp/die in a normal climate,
    this fails loudly.
  * ARCTIC → core temperature trends DOWN (cold works).
  * HUMID HEAT → core temperature trends UP (heat works).
  * ALL scenarios → no NaNs, every stat within absolute sane bounds, no
    spurious deaths in survivable environments.

It asserts INVARIANTS + TRENDS over a short run, not full death timelines
(those are covered by the per-phase manual probes). Runtime ~3-4 min.

Usage: python3 tools/physiology_probe.py [--port 9170] [--seconds 30] [--count 5]
Exit 0 = all scenarios passed.
"""
from __future__ import annotations
import argparse, glob, json, socket, subprocess, sys, time

LOG = "/tmp/physiology_probe_engine.log"


def send(port, lua, timeout=10.0):
    with socket.create_connection(("localhost", port), timeout=timeout) as s:
        s.sendall((lua + "\n").encode())
        chunks = []
        s.settimeout(0.5)
        try:
            while True:
                b = s.recv(4096)
                if not b:
                    break
                chunks.append(b)
        except socket.timeout:
            pass
    out = b"".join(chunks).decode(errors="replace")
    res = [ln[2:].strip() for ln in out.splitlines() if ln.startswith("> ")]
    res = [r for r in res if r]
    return (res[-1] if res else out.strip())


def boot(port):
    log = open(LOG, "w")
    proc = subprocess.Popen(
        ["cabal", "run", "-v0", "exe:synarchy", "--",
         "--headless", "--port", str(port)],
        stdout=log, stderr=subprocess.STDOUT)
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


def bootstrap(port):
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/infections/*.yaml", "engine.loadInfectionYaml"),
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
    # Neutralise the AI wander tick so units stay put — we're measuring the
    # physiology, not pathing.
    send(port, "pcall(function() require('scripts.unit_ai').update = "
               "function() end end); return 'ok'")
    send(port, "require('scripts.movement_arena').buildCourse('flat'); return 'ok'")


# One-line Lua chunk that aggregates the scenario's units (set in _U) into a
# table the debug console serialises to JSON.
AGG = (
    "local U=_U or {} local n=#U "
    "local function agg(s) local mn,mx,sm=1/0,-1/0,0 for _,u in ipairs(U) do "
    "local v=unit.getStat(u,s) or 0 if v<mn then mn=v end if v>mx then mx=v end "
    "sm=sm+v end return {min=mn,max=mx,avg=(n>0 and sm/n or 0)} end "
    "local dead=0 for _,u in ipairs(U) do if unit.getPose(u)=='dead' then "
    "dead=dead+1 end end "
    "local nan=false for _,u in ipairs(U) do for _,s in ipairs("
    "{'core_temp','salt_conc','circulation'}) do local v=unit.getStat(u,s) "
    "if v and v~=v then nan=true end end end "
    "return {n=n,dead=dead,core=agg('core_temp'),conc=agg('salt_conc'),"
    "circ=agg('circulation'),hypo=agg('hypothermia').max,"
    "hyper=agg('hyperthermia').max,salt=agg('salt_imbalance').max,nan=nan}"
)


def measure(port):
    raw = send(port, AGG)
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return {"_raw": raw}


def spawn_batch(port, count, species="acolyte", x0=1):
    # A small grid near the origin (in-bounds for the 'flat' course). x0 lets
    # two species share the strip without overlapping.
    pts = [(x0 + (i % 3), i // 3) for i in range(count)]
    ids = []
    for (x, y) in pts:
        r = send(port, f"local u=unit.spawn('{species}',{x},{y}); return u")
        try:
            ids.append(int(float(r)))
        except ValueError:
            pass
    send(port, "_U={" + ",".join(str(i) for i in ids) + "}; return #_U")
    return ids


def set_climate(port, ambient, humidity):
    send(port, f"local t=require('scripts.thermo'); t.debugAmbient={ambient}; "
               f"t.debugHumidity={humidity}; return 'set'")


def avg_core(port, ids):
    """Mean core_temp over an explicit id set (swaps the _U aggregate global)."""
    send(port, "_U={" + ",".join(str(i) for i in ids) + "}; return #_U")
    return measure(port)["core"]["avg"]


def unit_masses(port, uid):
    raw = send(port, f"local u={uid}; return string.format('%f,%f,%f',"
                     f"unit.getStat(u,'body_mass') or 0,unit.getStat(u,'fat_mass') or 0,"
                     f"unit.getStat(u,'lean_mass') or 0)")
    raw = raw.strip().strip('"')   # debug console quotes string returns
    try:
        return tuple(float(x) for x in raw.split(","))
    except ValueError:
        return None


def small_creature_section(port, seconds):
    """Coverage for the mass-scaling fix: a tiny creature must spawn coherent
    at its real mass (not force-floored), stay stable in a temperate climate,
    and lose heat FASTER than a human-scale unit (heat capacity ∝ mass)."""
    passed = True

    # 1. Temperate stability + body-composition coherence at real small mass.
    set_climate(port, 22, 0.5)
    sq = spawn_batch(port, 3, "red_squirrel")
    if not sq:
        print("  [FAIL] small creature: could not spawn red_squirrel")
        return False
    time.sleep(1.0)
    masses = unit_masses(port, sq[0])
    time.sleep(seconds)
    m = measure(port)
    bad = []
    if masses:
        bm, fm, lm = masses
        if bm > 5.0:        bad.append(f"body_mass {bm:.2f}kg force-floored (not small)")
        if bm <= fm + lm:   bad.append(f"incoherent body {bm:.3f} <= fat+lean {fm+lm:.3f}")
    else:
        bad.append("could not read masses")
    if m["dead"] != 0:                      bad.append(f"{m['dead']} died in temperate")
    if not (33.5 <= m["core"]["min"]):      bad.append(f"core too low {m['core']['min']:.1f}")
    if m["core"]["max"] > 39.0:             bad.append(f"core too high {m['core']['max']:.1f}")
    if not (0.7 <= m["conc"]["min"] and m["conc"]["max"] <= 1.3):
        bad.append(f"salt {m['conc']['min']:.2f}-{m['conc']['max']:.2f}")
    ok = not bad
    passed &= ok
    detail = (f"coherent {masses[0]:.2f}kg, stable" if ok else "; ".join(bad))
    print(f"  [{'PASS' if ok else 'FAIL'}] small creature temperate: {detail}")

    # 2. Cold response: in a cold climate, a squirrel (tiny heat capacity, no
    #    clothing) reaches a colder steady state — and reaches it fast — while a
    #    slow, clothed acolyte stays warm. We compare ABSOLUTE core after the
    #    window (not the drop: the squirrel equilibrates almost instantly, so a
    #    delayed baseline already misses the fall). Proves the inertia scaling +
    #    that a small creature is genuinely more cold-vulnerable. Mild cold so it
    #    settles colder without necessarily dying in the window.
    set_climate(port, 0, 0.5)
    sq2 = spawn_batch(port, 3, "red_squirrel", x0=1)
    ac2 = spawn_batch(port, 3, "acolyte", x0=5)
    time.sleep(min(seconds, 15.0))
    core_sq = avg_core(port, sq2)
    core_ac = avg_core(port, ac2)
    ok2 = core_sq < core_ac - 1.0    # the small unclothed creature ends up colder
    passed &= ok2
    print(f"  [{'PASS' if ok2 else 'FAIL'}] small creature more cold-vulnerable: "
          f"squirrel core {core_sq:.1f}°C vs acolyte {core_ac:.1f}°C")
    return passed


# ---- Scenario definitions ----
# Each: name, ambient °C, humidity 0..1, and an assert(m)->(ok, detail) on the
# measured aggregate.
def temperate_ok(m):
    bad = []
    if m["dead"] != 0: bad.append(f"{m['dead']} died")
    if not (35.5 <= m["core"]["min"] and m["core"]["max"] <= 38.5):
        bad.append(f"core {m['core']['min']:.1f}-{m['core']['max']:.1f}")
    if not (0.8 <= m["conc"]["min"] and m["conc"]["max"] <= 1.2):
        bad.append(f"salt {m['conc']['min']:.2f}-{m['conc']['max']:.2f}")
    if m["circ"]["min"] < 0.75: bad.append(f"circ min {m['circ']['min']:.2f}")
    return (not bad, "; ".join(bad) or "all normal")


def temperate_check(m, base):
    return temperate_ok(m)


def arctic_check(m, base):
    # Cold must move core DOWN over the window (real thermal inertia is slow, so
    # we check the direction vs the scenario's own baseline, not a magnitude).
    drop = base["core"]["avg"] - m["core"]["avg"]
    if drop > 0.2 or m["hypo"] > 0:
        return True, f"core −{drop:.2f}°C (cooling), hypo {m['hypo']:.2f}"
    return False, f"core only −{drop:.2f}°C — not cooling"


def humid_heat_check(m, base):
    rise = m["core"]["avg"] - base["core"]["avg"]
    if rise > 0.1 or m["hyper"] > 0:
        return True, f"core +{rise:.2f}°C (heating), hyper {m['hyper']:.2f}"
    return False, f"core only +{rise:.2f}°C — not heating"


def survivable_check(m, base):
    # Dry heat / mild cold: shouldn't kill over the short run; just sane.
    return (m["dead"] == 0, f"{m['dead']} died" if m["dead"] else "survived")


SCENARIOS = [
    # Acolytes spawn CLOTHED (insulation ~1.95), so the cold scenario must be
    # extreme enough to overwhelm clothing + max shivering — that's the point
    # (clothing protects; deep cold still kills).
    ("temperate (22C/0.5)",   22, 0.5,  temperate_check),
    ("mild cold (5C/0.5)",     5, 0.5,  survivable_check),
    ("deep arctic (-55C/0.6)", -55, 0.6,  arctic_check),
    ("dry heat (45C/0.15)",   45, 0.15, survivable_check),
    ("humid heat (46C/0.95)", 46, 0.95, humid_heat_check),
]


def sane_bounds(m):
    bad = []
    if m.get("nan"): bad.append("NaN stat")
    if not (15 <= m["core"]["min"] and m["core"]["max"] <= 45):
        bad.append(f"core out of bounds {m['core']['min']:.1f}-{m['core']['max']:.1f}")
    if not (0 <= m["conc"]["min"] and m["conc"]["max"] <= 3.0):
        bad.append(f"salt_conc out of bounds {m['conc']['min']:.2f}-{m['conc']['max']:.2f}")
    if not (0 <= m["circ"]["min"] and m["circ"]["max"] <= 1.01):
        bad.append(f"circ out of bounds {m['circ']['min']:.2f}-{m['circ']['max']:.2f}")
    return (not bad, "; ".join(bad) or "bounds ok")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9170)
    ap.add_argument("--seconds", type=float, default=30.0)
    ap.add_argument("--count", type=int, default=5)
    args = ap.parse_args()
    port = args.port
    proc = boot(port)
    passed = True
    try:
        bootstrap(port)
        for name, ambient, humidity, check in SCENARIOS:
            set_climate(port, ambient, humidity)
            spawn_batch(port, args.count)
            time.sleep(1.0)
            base = measure(port)        # per-scenario baseline (≈ spawn state)
            time.sleep(args.seconds)
            m = measure(port)
            if "_raw" in m or "_raw" in base:
                print(f"  [FAIL] {name}: bad measurement: "
                      f"{m.get('_raw', base.get('_raw',''))[:120]}")
                passed = False
                continue
            b_ok, b_detail = sane_bounds(m)
            s_ok, s_detail = check(m, base)
            ok = b_ok and s_ok
            passed &= ok
            print(f"  [{'PASS' if ok else 'FAIL'}] {name}: {s_detail}"
                  + ("" if b_ok else f" | BOUNDS: {b_detail}"))
        passed &= small_creature_section(port, args.seconds)
        print("\n" + ("ALL SCENARIOS PASSED" if passed else "SOME FAILED"))
    finally:
        try:
            send(port, "engine.quit()", timeout=3)
        except Exception:
            pass
        time.sleep(1.0)
        if proc.poll() is None:
            proc.kill()
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
