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


def _calories(port, uid):
    r = send(port, f"return unit.getCalories({uid}) or -1")
    try:
        return float(r)
    except ValueError:
        return None


def _stomach(port, uid):
    r = send(port, f"return unit.getStat({uid},'hunger') or -1")
    try:
        return float(r)
    except ValueError:
        return None


def _sack_fill(port, uid):
    r = send(port, f"local inv=unit.getInventory({uid}) or {{}}; "
                   f"for _,it in ipairs(inv) do "
                   f"if it.defName=='quinoa_sack' then return it.currentFill end end; "
                   f"return -1")
    try:
        return float(r)
    except ValueError:
        return None


def hunger_section(port, seconds):
    """Two-layer food system (#92/#93) regression coverage:
      * Activity-scaled drain — a unit in combat burns its calorie STORE
        AND water faster than an idle one (combat detected from currentAnim).
      * unit.feed fills the STOMACH meter from a carried ration; digestion
        then moves the meal into the calorie store over time; feed refuses
        an item the unit doesn't carry.
      * Bulk food — a quinoa sack spawns full (default_fill), feed draws
        just enough kg of fill to top the stomach up, and an eaten-dry
        sack is removed from the inventory.
      * Calorie→thermoregulation coupling — a starving unit in the cold
        generates less metabolic heat and ends colder than a fed one.
    All on the flat temperate arena, AI wander already neutralised."""
    passed = True
    win = min(seconds, 20.0)

    # --- 1. Activity-scaled drain: idle vs forced-combat ---
    # Cool (not warm) climate so clothed acolytes don't sweat — that keeps
    # hydration drain to the activity-scaled metabolic baseline we're
    # measuring, instead of being swamped by per-unit sweat noise.
    set_climate(port, 14, 0.4)
    idle = spawn_batch(port, 2, "acolyte", x0=1)
    actv = spawn_batch(port, 2, "acolyte", x0=8)
    if not idle or not actv:
        print("  [FAIL] hunger: could not spawn acolytes")
        return False
    # Drop the STORE below the 75%-surplus-regrowth band so we measure the
    # bare metabolic burn, EMPTY the stomach so digestion doesn't top the
    # store back up mid-measurement, and strip rations so nothing refills.
    for u in idle + actv:
        send(port, f"local u={u}; local mc=unit.getStat(u,'max_calories'); "
                   f"unit.setStat(u,'calories',mc*0.5); "
                   f"unit.setStat(u,'hunger',0); "
                   f"unit.removeItem(u,'rations'); unit.removeItem(u,'rations'); "
                   f"return 'ok'")
    # Keep the active group in a combat anim: attack_quick is a ~0.4s
    # one-shot, so re-assert it continuously (each batched send takes ~the
    # console read-timeout, so the loop naturally re-fires at roughly the
    # anim cadence → currentAnim stays a combat anim → 2.5× metabolic +
    # hydration drain). The idle group keeps its default idle anim (1.0×).
    actv_lua = "{" + ",".join(str(u) for u in actv) + "}"
    def force_combat():
        send(port, f"for _,u in ipairs({actv_lua}) do "
                   f"unit.setAnimOverride(u,'attack_quick') end; return 'ok'")
    force_combat()
    time.sleep(0.5)
    def avg_hyd(group):
        return sum(float(send(port, f"return unit.getStat({u},'hydration') or 0"))
                   for u in group) / len(group)
    h_idle0 = sum(_calories(port, u) for u in idle) / len(idle)
    h_actv0 = sum(_calories(port, u) for u in actv) / len(actv)
    w_idle0 = avg_hyd(idle)
    w_actv0 = avg_hyd(actv)
    t_end = time.time() + win
    while time.time() < t_end:
        force_combat()
        time.sleep(0.2)   # leave the Lua thread room to run its resource tick
    h_idleD = h_idle0 - sum(_calories(port, u) for u in idle) / len(idle)
    h_actvD = h_actv0 - sum(_calories(port, u) for u in actv) / len(actv)
    w_idleD = w_idle0 - avg_hyd(idle)
    w_actvD = w_actv0 - avg_hyd(actv)
    ok1 = h_idleD > 0 and h_actvD > h_idleD * 1.3
    passed &= ok1
    print(f"  [{'PASS' if ok1 else 'FAIL'}] calorie store drains faster in combat: "
          f"idle −{h_idleD:.1f} kcal vs combat −{h_actvD:.1f} kcal")
    ok1b = w_actvD > w_idleD       # hydration also activity-scaled
    passed &= ok1b
    print(f"  [{'PASS' if ok1b else 'FAIL'}] hydration drains faster in combat: "
          f"idle −{w_idleD:.4f} L vs combat −{w_actvD:.4f} L")

    # --- 2. unit.feed fills the stomach; digestion moves it into the
    #        store; feed refuses what isn't carried ---
    fed = spawn_batch(port, 1, "acolyte", x0=14)[0]
    # Clean slate: empty stomach, half-full store — one meal's journey is
    # then visible on both meters.
    send(port, f"local u={fed}; unit.setStat(u,'hunger',0); "
               f"unit.setStat(u,'calories',unit.getStat(u,'max_calories')*0.5); "
               f"return 'ok'")
    before = _stomach(port, fed)
    credited = send(port, f"return unit.feed({fed},'rations') or -1")
    after = _stomach(port, fed)
    try:
        cred = float(credited)
    except ValueError:
        cred = -1
    ok2 = cred > 0 and after > before
    passed &= ok2
    print(f"  [{'PASS' if ok2 else 'FAIL'}] unit.feed fills the stomach: "
          f"+{cred:.0f} kcal ({before:.0f}→{after:.0f})")
    # Digestion: over a short window the stomach drains and the store
    # rises (transfer ~3 kcal/s dwarfs the ~0.9 kcal/s idle burn).
    st0, ca0 = _stomach(port, fed), _calories(port, fed)
    time.sleep(8.0)
    st1, ca1 = _stomach(port, fed), _calories(port, fed)
    ok2d = st1 < st0 - 2 and ca1 > ca0 + 2
    passed &= ok2d
    print(f"  [{'PASS' if ok2d else 'FAIL'}] digestion moves stomach→store: "
          f"stomach {st0:.0f}→{st1:.0f}, store {ca0:.0f}→{ca1:.0f}")
    # Feeding an item the unit doesn't carry returns nil.
    no_carry = send(port, f"return unit.feed({fed},'nonexistent_food') and 'num' or 'nil'")
    ok2b = no_carry.strip().strip('"') == "nil"
    passed &= ok2b
    print(f"  [{'PASS' if ok2b else 'FAIL'}] unit.feed refuses non-carried item: {no_carry.strip()}")

    # --- 2c. Bulk food (#93): quinoa sack spawns full via default_fill;
    #         feed draws only the stomach deficit's worth of kg; an
    #         eaten-dry sack is removed from the inventory.
    bulk = spawn_batch(port, 1, "acolyte", x0=17)[0]
    send(port, f"local u={bulk}; unit.removeItem(u,'rations'); "
               f"unit.removeItem(u,'rations'); unit.setStat(u,'hunger',0); "
               f"return 'ok'")
    send(port, f"unit.addItem({bulk},'quinoa_sack'); return 'ok'")
    fill0 = _sack_fill(port, bulk)
    cred_bulk_s = send(port, f"return unit.feed({bulk},'quinoa_sack') or -1")
    fill1 = _sack_fill(port, bulk)
    st_bulk = _stomach(port, bulk)
    max_hun = float(send(port, f"return unit.getStat({bulk},'max_hunger') or -1"))
    try:
        cred_bulk = float(cred_bulk_s)
    except ValueError:
        cred_bulk = -1
    kg_drawn = (fill0 or 0) - (fill1 or 0)
    # The credit must equal the full deficit (stomach was emptied right
    # before the feed) and the kg drawn must match it at 3680 kcal/kg.
    # The stomach READ is digestion-slack tolerant — it starts draining
    # into the store the moment the meal lands (~3 kcal/s across the
    # probe's send round-trips), so compare against the credit, not
    # equality with max_hunger.
    ok2c = (fill0 is not None and abs(fill0 - 5.0) < 1e-3        # default_fill
            and cred_bulk > 0
            and abs(cred_bulk - max_hun) < 1.0                   # full deficit
            and st_bulk > max_hun * 0.9                          # topped up
            and abs(kg_drawn - cred_bulk / 3680.0) < 1e-3)       # mass ↔ kcal
    passed &= ok2c
    print(f"  [{'PASS' if ok2c else 'FAIL'}] bulk feed draws stomach deficit from sack: "
          f"fill {fill0}→{fill1} kg (−{kg_drawn:.3f}), +{cred_bulk:.0f} kcal, "
          f"stomach {st_bulk:.0f}/{max_hun:.0f}")
    # Drain-to-removal: swap in a nearly-empty sack (explicit tiny fill
    # overrides default_fill), empty the stomach, eat it dry.
    send(port, f"local u={bulk}; unit.removeItem(u,'quinoa_sack'); "
               f"unit.addItem(u,'quinoa_sack',0.02); "
               f"unit.setStat(u,'hunger',0); return 'ok'")
    cred_dry_s = send(port, f"return unit.feed({bulk},'quinoa_sack') or -1")
    fill_after = _sack_fill(port, bulk)   # -1 = no sack left
    try:
        cred_dry = float(cred_dry_s)
    except ValueError:
        cred_dry = -1
    ok2e = abs(cred_dry - 0.02 * 3680.0) < 1.0 and fill_after == -1
    passed &= ok2e
    print(f"  [{'PASS' if ok2e else 'FAIL'}] eaten-dry sack is removed: "
          f"+{cred_dry:.0f} kcal, sack {'gone' if fill_after == -1 else 'STILL PRESENT'}")

    # --- 3. Calorie→heat coupling: a starving body can't fuel shivering
    #        (or full basal burn), so cold-stressed it defends its
    #        temperature worse and chills faster. Both groups start ALREADY
    #        cold (core pre-set into the shivering zone) so the calorie-gated
    #        shivering — the dominant heat term down here — actually drives
    #        the divergence within the window. Same start core for both, so
    #        compare final cores directly.
    set_climate(port, -30, 0.5)
    starv = spawn_batch(port, 4, "acolyte", x0=1)
    feda  = spawn_batch(port, 4, "acolyte", x0=8)
    for u in starv:
        # Empty STORE (thermo gates on it) and empty stomach so digestion
        # can't quietly refill the store mid-window.
        send(port, f"local u={u}; unit.setStat(u,'calories',1); "
                   f"unit.setStat(u,'hunger',0); "
                   f"unit.setStat(u,'core_temp',34.0); return 'ok'")
    for u in feda:
        send(port, f"local u={u}; unit.setStat(u,'calories',unit.getStat(u,'max_calories')); "
                   f"unit.setStat(u,'core_temp',34.0); return 'ok'")
    time.sleep(max(seconds, 45.0))
    core_starv = avg_core(port, starv)
    core_fed   = avg_core(port, feda)
    ok3 = core_fed - core_starv > 0.1
    passed &= ok3
    print(f"  [{'PASS' if ok3 else 'FAIL'}] starving unit makes less heat (colder when cold-stressed): "
          f"starving {core_starv:.2f}°C vs fed {core_fed:.2f}°C")

    # --- 4. Wildlife guard: a unit with no food system (bear — its body
    #        block seeds max_hunger/max_calories, but it has no hunger or
    #        calories RESOURCE) must not be feedable (that would conjure a
    #        permanent, never-draining stomach pool) and reports no calorie
    #        reading. Regression guard for the heal/thermo gating keying
    #        off a live "calories" stat and feed gating off a live
    #        "hunger" stat, not the always-seeded maxima.
    bear = spawn_batch(port, 1, "bear_brown", x0=14)
    if bear:
        b = bear[0]
        send(port, f"unit.addItem({b},'rations',0); return 'ok'")
        fed_wild = send(port, f"return unit.feed({b},'rations') and 'num' or 'nil'").strip().strip('"')
        cal_wild = send(port, f"return unit.getCalories({b}) and 'num' or 'nil'").strip().strip('"')
        retained = send(port, f"local inv=unit.getInventory({b}) or {{}}; local n=0; "
                              f"for _,it in ipairs(inv) do if it.defName=='rations' then n=n+1 end end; "
                              f"return n").strip().strip('"')
        ok4 = fed_wild == "nil" and cal_wild == "nil" and retained == "1"
        passed &= ok4
        print(f"  [{'PASS' if ok4 else 'FAIL'}] wildlife has no calorie pool / isn't feedable: "
              f"feed={fed_wild} getCalories={cal_wild} ration_retained={retained}")
    else:
        print("  [WARN] wildlife guard: could not spawn bear_brown")
        bear = []

    # Clean up this section's units so they don't add tick load to the
    # climate scenarios that follow.
    for u in idle + actv + [fed, bulk] + starv + feda + bear:
        send(port, f"unit.destroy({u}); return 'ok'")
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
        # Run the hunger section FIRST, on a fresh engine with the fewest
        # accumulated units — the per-unit resource tick that drives calorie
        # / hydration drain gets starved if it runs after dozens of units
        # have piled up across the climate scenarios.
        passed &= hunger_section(port, args.seconds)
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
