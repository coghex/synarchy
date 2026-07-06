#!/usr/bin/env python3
"""State-of-mind probe (#350).

Drives the REAL engine's brain.lua — now the unified consciousness + mood
model — to confirm:

  1. A fresh unit reads as fully alert/content: consciousness, mood,
     concentration, and state_of_mind all 1.0, emotional_pain 0.0.
  2. Physical pain immediately depresses concentration (an instantaneous,
     non-integrated formula — checked against the exact expected value)
     and, over a few ticks, drags mood down and emotional_pain up (the
     "ache lingers" asymmetric-drift psychological layer).
  3. A species with no hunger/hydration config (bear_brown) computes all
     five values without error — the fracOf fallback (absent resource ⇒
     neutral, not penalized).
  4. THE REGRESSION GUARD this issue calls for: even once pain has
     tanked mood/emotional_pain/state_of_mind, the unit stays standing
     and brain.isUnconscious/isDelirious/isConfused all stay false — the
     locomotor collapse machine and the AI's delirium gate key on
     consciousness ALONE, unaffected by the new psychological layer.
     (physiological collapse itself is separately covered end-to-end by
     tools/collapse_crawl_probe.py and tools/concussion_revive_probe.py,
     which this change must also keep passing.)

Usage: python3 tools/state_of_mind_probe.py [--port 9350]
Exit 0 = pass.
"""
from __future__ import annotations
import argparse, glob, json, socket, subprocess, sys, time

LOG = "/tmp/state_of_mind_probe_engine.log"
PAIN_CEILING = 5.0  # brain.lua PAIN_CEILING — keep in lockstep


def send(port, lua, timeout=10.0, idle=0.3):
    with socket.create_connection(("localhost", port), timeout=timeout) as s:
        s.sendall((lua + "\n").encode())
        chunks = []
        s.settimeout(idle)
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
    return res[-1] if res else out.strip()


def boot(port):
    log = open(LOG, "w")
    proc = subprocess.Popen(
        ["cabal", "run", "-v0", "exe:synarchy", "--",
         "--headless", "--port", str(port)],
        stdout=log, stderr=subprocess.STDOUT)
    deadline = time.time() + 300
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
    # mental values, not pathing (the delirium/confusion gate is checked via
    # brain.* predicates directly, not by watching AI behaviour).
    send(port, "pcall(function() require('scripts.unit_ai').update = "
               "function() end end); return 'ok'")
    send(port, "require('scripts.movement_arena').buildCourse('flat'); return 'ok'")


def summary(port, uid):
    raw = send(port, f"return require('scripts.brain').summary({uid})")
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return {"_raw": raw}


def close(a, b, tol=0.02):
    return abs(a - b) <= tol


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9350)
    args = ap.parse_args()
    P = args.port

    proc = boot(P)
    ok = True
    try:
        bootstrap(P)

        # ---- 1. Fresh unit reads as fully alert/content. ----
        uid = send(P, "local u=unit.spawn('acolyte',1,0); return u")
        uid = int(float(uid))
        print(f"spawned acolyte uid={uid}")
        s0 = summary(P, uid)
        print(f"  fresh: {s0}")
        fresh_ok = (s0.get("state") == "alert"
                    and close(s0.get("consciousness", 0), 1.0)
                    and close(s0.get("mood", 0), 1.0)
                    and close(s0.get("emotionalPain", 1), 0.0)
                    and close(s0.get("concentration", 0), 1.0)
                    and close(s0.get("stateOfMind", 0), 1.0))
        if fresh_ok:
            print("  [pass] fresh unit: alert, mood/concentration/stateOfMind ~1.0, no emotional pain")
        else:
            ok = False
            print(f"  [FAIL] fresh unit not at baseline: {s0}")

        # ---- 3. Species with no hunger/hydration config computes cleanly. ----
        bear = send(P, "local u=unit.spawn('bear_brown',5,0); return u")
        bear = int(float(bear))
        sb = summary(P, bear)
        print(f"  bear_brown (no hunger config) uid={bear}: {sb}")
        bear_ok = (sb.get("state") == "alert"
                   and all(isinstance(sb.get(k), (int, float))
                           for k in ("consciousness", "mood", "emotionalPain",
                                     "concentration", "stateOfMind")))
        if bear_ok:
            print("  [pass] hunger-less species computes all mental values (fallback, no crash)")
        else:
            ok = False
            print(f"  [FAIL] bear_brown summary malformed: {sb}")

        # ---- 2. Physical pain: instantaneous concentration hit + drift. ----
        send(P, f"return unit.injure({uid},'l_forearm','slash',0.6,0.0)")
        rawpain = send(P, f"return unit.getPain({uid})")
        pain_raw = float(rawpain)
        pain_frac = max(0.0, min(1.0, pain_raw / PAIN_CEILING))
        time.sleep(0.5)
        s1 = summary(P, uid)
        expected_conc = 1.0 * (1.0 - 0.6 * pain_frac) * 1.0  # stamina still ~full
        print(f"  injured (raw pain={pain_raw:.3f}, frac={pain_frac:.3f}): {s1}")
        if close(s1.get("concentration", 0), expected_conc, tol=0.05):
            print(f"  [pass] concentration drops to ~{expected_conc:.3f} immediately on injury")
        else:
            ok = False
            print(f"  [FAIL] concentration {s1.get('concentration')} != expected ~{expected_conc:.3f}")

        # Let pain persist a few seconds — emotional_pain should climb toward
        # pain_frac (fast rise), mood should drop below its fresh baseline.
        samples = []
        for _ in range(8):
            time.sleep(0.5)
            samples.append(summary(P, uid))
        ep_trend = [s.get("emotionalPain", 0) for s in samples]
        mood_trend = [s.get("mood", 1) for s in samples]
        print(f"  emotionalPain trend: {[round(x,3) for x in ep_trend]}")
        print(f"  mood trend:          {[round(x,3) for x in mood_trend]}")

        rising = ep_trend[-1] > ep_trend[0] and ep_trend[-1] <= pain_frac + 0.05
        if rising:
            print(f"  [pass] emotional_pain rises toward pain fraction ({pain_frac:.3f}) over sustained pain")
        else:
            ok = False
            print(f"  [FAIL] emotional_pain didn't rise sensibly: {ep_trend}")

        mood_down = mood_trend[-1] < mood_trend[0] - 0.002
        if mood_down:
            print(f"  [pass] mood drifts down under sustained pain "
                  f"({mood_trend[0]:.4f} → {mood_trend[-1]:.4f})")
        else:
            ok = False
            print(f"  [FAIL] mood didn't drift down under sustained pain: {mood_trend}")

        # ---- 4. Regression guard: psychological tanking must NOT trip the
        # physiological collapse/delirium gates — those stay keyed on
        # consciousness alone. (One table return, JSON-decoded, so the
        # console's string-quoting doesn't need string-compare gymnastics.)
        s2 = samples[-1]
        gate = json.loads(send(P,
            f"local u={uid} local b=require('scripts.brain') "
            f"return {{pose=unit.getPose(u), uncon=b.isUnconscious(u), "
            f"delir=b.isDelirious(u), conf=b.isConfused(u), state=b.state(u)}}"))
        print(f"  post-pain gate check: state_of_mind={s2.get('stateOfMind'):.3f} gate={gate}")
        guard_ok = (gate["pose"] == "standing" and not gate["uncon"] and not gate["delir"]
                    and not gate["conf"] and gate["state"] == "alert"
                    and s2.get("stateOfMind", 1.0) < s2.get("consciousness", 1.0))
        if guard_ok:
            print("  [pass] REGRESSION GUARD: collapse/delirious/confused gates stay off "
                  "consciousness alone — unaffected by the depressed psychological layer "
                  "(state_of_mind < consciousness, but pose/gates untouched)")
        else:
            ok = False
            print(f"  [FAIL] psychological layer leaked into physiological gating: gate={gate}")

        print(f"\n{'PASS' if ok else 'FAIL'} — unified state-of-mind model (#350)")
        return 0 if ok else 1
    finally:
        send(P, "engine.quit(); return 'bye'")
        time.sleep(0.5)
        if proc.poll() is None:
            proc.kill()


if __name__ == "__main__":
    sys.exit(main())
