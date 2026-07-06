#!/usr/bin/env python3
"""Thought system probe (#351).

The thought log PANEL surfacing is GUI (unit_log.lua's new Thought tab)
and can't be verified headless, but the data path that feeds it is pure
engine + Lua plumbing and IS testable — same reasoning as
tools/injury_log_probe.py, which this mirrors.

`--headless` still runs the real scripts/init.lua boot, so
unit_resources.lua (and thus thoughts.tick) and thought_log.lua are ALREADY
auto-loaded and ticking in the background for every spawned unit — same
situation injury_log_probe.py is in with injury_log_panel.lua. Every check
below therefore fires + drains in ONE atomic console round-trip (a single
Lua chunk can't be interleaved by another script's tick), never split
across two send() calls — splitting them left a real race in an earlier
draft where the background loop's own drain silently ate the event first.

Checks:

  1. thought.emit / thought.drainEvents roundtrip (+ drain clears the
     buffer) — the new engine-side event stream (thoughtEventsRef).
  2. data/thoughts.yaml loads via scripts/thoughts.loadCatalogue().
  3. STATE thought: a unit in high pain rolls a "state"-category thought
     within one forced tick, and its mood_delta measurably moves "mood".
  4. ENVIRONMENTAL thought: with world.getAmbientAt monkey-patched to an
     arctic reading, a unit rolls an "environmental"-category thought.
  5. State-of-mind biases selection: with a synthetic two-entry catalogue
     (one negative-valence, one positive-valence, equal base weight,
     neither triggered), a low-mood unit draws the negative entry far
     more often than a high-mood unit does — the two-way loop's "state
     of mind -> thoughts" half.
  6. Thought-log data path: scripts/thought_log.lua drains a fresh
     thought.emit and surfaces it via unitEntries(uid) — the store
     scripts/unit_log.lua's Thought tab reads (tab-merge itself is GUI,
     not gated here, matching injury_log_probe's scope boundary).

Usage: python3 tools/thought_probe.py [--port 9351]
Exit 0 = pass.
"""
from __future__ import annotations
import argparse, glob, socket, subprocess, sys, time

LOG = "/tmp/thought_probe_engine.log"


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
    return (res[-1] if res else out.strip()).strip('"')


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
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    send(port, "engine.loadScript('scripts/unit_stats.lua', 0.1); return 'ok'")
    # unit_ai's wander is neutralised so gridX/gridY (read by the
    # environmental trigger) stay put; unit_resources/thought_log are
    # already running via the real init.lua boot (see module docstring).
    send(port, "pcall(function() require('scripts.unit_ai').update = "
               "function() end end); return 'ok'")
    send(port, "require('scripts.movement_arena').buildCourse('flat'); return 'ok'")


def check(name, ok, detail=""):
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}" + (f"  ({detail})" if detail else ""))
    return ok


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9351)
    args = ap.parse_args()
    P = args.port

    proc = boot(P)
    passed = True
    try:
        bootstrap(P)

        print("1. thought.emit / drainEvents roundtrip")
        r = send(P,
            "thought.emit(7,'a stray thought','random'); "
            "local e=thought.drainEvents(); if #e<1 then return 'NONE' end; "
            "return e[1].target..'|'..e[1].kind..'|'..(e[1].payload.text or '?')")
        passed &= check("emit then drain returns the event",
                         r == "7|random|a stray thought", r)
        r2 = send(P, "return #thought.drainEvents()")
        passed &= check("second drain is empty", r2 == "0", r2)

        print("2. data/thoughts.yaml loads")
        n = send(P, "return require('scripts.thoughts').loadCatalogue()")
        passed &= check("catalogue has entries", int(float(n)) >= 10, n)

        # A category being ELIGIBLE doesn't mean it wins any given roll —
        # it competes in the same weighted pool as the always-eligible
        # "random" entries (that's by design: a cold environment doesn't
        # suppress ambient thoughts, it just adds to the pool). So poll a
        # few atomic rolls and accept the first one that lands the
        # expected category, rather than asserting on a single roll.
        def roll_until(uid, want_prefix, attempts=20):
            for _ in range(attempts):
                r = send(P,
                    f"unit.setStat({uid},'thought_next_at',0); "
                    f"require('scripts.thoughts').tick({uid}, unit.getInfo({uid}), 0.2); "
                    f"local e=thought.drainEvents(); if #e<1 then return 'NONE' end; "
                    f"return e[1].kind..'|'..e[1].payload.text")
                if r.startswith(want_prefix):
                    return r
            return r

        print("3. STATE thought: high pain -> 'state' category + mood moves")
        uid = int(float(send(P, "local u=unit.spawn('acolyte',1,0); return u")))
        time.sleep(0.8)
        # Fraction, not raw severity: painFrac = getPain()/PAIN_CEILING(5.0),
        # so pain_high's >0.5 threshold needs several wounds, not one hit.
        for part in ("l_forearm", "r_forearm", "l_thigh", "r_thigh", "torso"):
            send(P, f"return unit.injure({uid},'{part}','slash',0.9,0.0)")
        moodBefore = float(send(P, f"return require('scripts.brain').mood({uid})"))
        r = roll_until(uid, "state|")
        moodAfter = float(send(P, f"return require('scripts.brain').mood({uid})"))
        print(f"  fired: {r}  mood {moodBefore:.4f} -> {moodAfter:.4f}")
        passed &= check("fired a 'state' thought", r.startswith("state|"), r)
        passed &= check("mood measurably moved", abs(moodAfter - moodBefore) > 0.005,
                         f"{moodBefore:.4f} -> {moodAfter:.4f}")

        print("4. ENVIRONMENTAL thought: arctic ambient -> 'environmental' category")
        uid2 = int(float(send(P, "local u=unit.spawn('acolyte',3,0); return u")))
        time.sleep(0.8)
        send(P, "_ORIG_AMBIENT = world.getAmbientAt; "
                "world.getAmbientAt = function(gx,gy) return -15.0 end; return 'ok'")
        r = roll_until(uid2, "environmental|")
        send(P, "world.getAmbientAt = _ORIG_AMBIENT; return 'ok'")
        print(f"  fired: {r}")
        passed &= check("fired an 'environmental' thought", r.startswith("environmental|"), r)

        print("5. state of mind biases selection (mood-weighted valence)")
        send(P, "require('scripts.thoughts').catalogue = {"
                "{id='neg',valence='negative',weight=1,mood_delta=0,text='NEG'},"
                "{id='pos',valence='positive',weight=1,mood_delta=0,text='POS'},"
                "}; return 'ok'")
        uid3 = int(float(send(P, "local u=unit.spawn('acolyte',5,0); return u")))
        time.sleep(0.8)

        def neg_fraction(mood, n=60):
            send(P, f"unit.setStat({uid3},'mood',{mood}); "
                    f"_NEG={{}}; _TOTAL={{}}; return 'ok'")
            for i in range(n):
                # Fire + drain + tally in ONE round-trip per roll — a
                # drain split into its own later send() would race the
                # background thought_log tick eating the event first
                # (see module docstring).
                send(P,
                    f"unit.setStat({uid3},'thought_next_at',0); "
                    f"require('scripts.thoughts').tick({uid3}, unit.getInfo({uid3}), 0.2); "
                    f"local e=thought.drainEvents(); "
                    f"for _,ev in ipairs(e) do _TOTAL[#_TOTAL+1]=1; "
                    f"if ev.payload.text=='NEG' then _NEG[#_NEG+1]=1 end end; "
                    f"return 'ok'")
            r = send(P, "return #_NEG..'|'..#_TOTAL")
            neg_n, total = (int(x) for x in r.split("|"))
            return (neg_n / total) if total else 0.0

        frac_low  = neg_fraction(0.05)
        frac_high = neg_fraction(0.95)
        print(f"  negative-pick fraction: mood=0.05 -> {frac_low:.2f}, "
              f"mood=0.95 -> {frac_high:.2f}")
        passed &= check("low mood draws negative valence far more than high mood",
                         frac_low - frac_high > 0.3,
                         f"low={frac_low:.2f} high={frac_high:.2f}")

        print("6. thought_log.lua data path: emit -> update() -> unitEntries()")
        send(P, "engine.loadScript('scripts/thought_log.lua', 0.1); return 'ok'")
        time.sleep(0.3)
        send(P, f"thought.emit({uid3},'PROBE_CHECK_TEXT','random'); return 'ok'")
        send(P, "require('scripts.thought_log').update(0.1); return 'ok'")
        r = send(P,
            f"local es=require('scripts.thought_log').unitEntries({uid3}) "
            f"if #es<1 then return 'NONE' end return es[1].text")
        passed &= check("thought_log surfaces the emitted text",
                         "PROBE_CHECK_TEXT" in r, r)

        print(f"\n{'PASS' if passed else 'FAIL'} — thought system (#351)")
        return 0 if passed else 1
    finally:
        try:
            send(P, "engine.quit()", timeout=2)
        except OSError:
            pass
        time.sleep(1)
        if proc.poll() is None:
            proc.kill()


if __name__ == "__main__":
    sys.exit(main())
