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
     arctic reading, a unit rolls THE COLD THOUGHT specifically (#1759) —
     not merely something in the "environmental" category. Four entries
     share that category behind four independent triggers, and the day/
     night predicates partition every non-nil sun angle, so one of them
     is always eligible beside `cold`; asserting the category alone
     passes on the daylight thought while no cold thought ever fired.
     The phase therefore establishes every precondition that decides
     which environmental entries compete (ambient, sun angle, and the
     unit's mood, the last re-applied per roll because brain.tick drifts
     it back between round trips) and asserts on the emitted text, the
     identity thought.emit actually publishes. Selection itself is
     untouched: the cold thought still has to win a normal weighted roll
     against the day thought and the always-eligible "random" entries.
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

This probe implements the shared `probe-result/v1` contract: `--describe`
prints its ordered stable checks without booting an engine, and a harnessed
run writes structured events while a standalone run keeps its human-readable
per-check output.
"""
from __future__ import annotations
import argparse, glob, socket, subprocess, sys, time
import probe_protocol
from probelib import quit_engine, boot, send

LOG = "/tmp/thought_probe_engine.log"
LOG_NAME = "thought_probe_engine.log"
PROBE_KEY = "thought"
PROBE_CHECKS = [
    ("emit_roundtrip", "emit then drain returns the event"),
    ("drain_destructive", "second drain is empty"),
    ("catalogue_loaded", "catalogue has entries"),
    ("state_thought_fired", "fired a 'state' thought"),
    ("state_thought_moves_mood", "mood measurably moved"),
    ("cold_thought_fired", "fired the COLD environmental thought"),
    ("world_patches_restored", "phase 4's world patches are restored"),
    ("mood_biases_valence", "low mood draws negative valence far more than high mood"),
    ("thought_log_surfaces_text", "thought_log surfaces the emitted text"),
]
DESCRIPTOR = probe_protocol.build_descriptor(PROBE_KEY, PROBE_CHECKS)
CHECK_ID_BY_LABEL = {label: check_id for check_id, label in PROBE_CHECKS}
_REPORTER: probe_protocol.Reporter | None = None

# The exact identity phase 4 asserts on. thought.emit publishes category
# and text only (scripts/thoughts.lua's tick) — never the catalogue
# entry's `id` — so the text IS the identity available on the wire, and
# this is data/thoughts.yaml's `cold_bite` verbatim.
COLD_THOUGHT = "environmental|The cold bites at any exposed skin."

# Phase 4's controlled preconditions.
#   -15.0 C  makes `cold` eligible and `hot` not.
#   sunAngle 0.5 is noon, so `day` is the one deterministic environmental
#            competitor and `night` never is. The two predicates partition
#            every non-nil angle, so one of them is ALWAYS eligible: the
#            point is to fix WHICH one competes, not to remove it.
#   mood 0.5 is moodBiasFactor's neutral point — every valence scores
#            exactly 1.0 there, so no entry gets a thumb on the scale in
#            either direction, and the value sits well inside the stable
#            band (mental_state's STRESSED_BELOW 0.35 / EUPHORIC_ABOVE
#            0.90) so no mental-state entries join the pool either.
COLD_AMBIENT_C = -15.0
NOON_SUN_ANGLE = 0.5
PHASE4_MOOD = 0.5

# The cold thought must still WIN a normal weighted roll against the day
# thought and the four always-eligible "random" entries, so the poll runs
# long enough for that to be a near-certainty rather than a coin flip.
# At the weights above it takes ~8 rolls on average; the cap is only ever
# paid on a genuine failure.
PHASE4_ATTEMPTS = 120


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
    if _REPORTER is None:
        raise RuntimeError("thought reporter is not initialised")
    payload = {"detail": str(detail)} if detail else None
    return _REPORTER.check(CHECK_ID_BY_LABEL[name], bool(ok), name, payload)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9351)
    ap.add_argument("--describe", action="store_true")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(args.port, rep)
    finally:
        rep.close()


def _run(P: int, rep: probe_protocol.Reporter) -> int:
    global _REPORTER
    _REPORTER = rep
    proc = boot(P, log=rep.engine_log_path(LOG_NAME, LOG),
                args=rep.engine_args())
    passed = True
    try:
        bootstrap(P)

        rep.note("1. thought.emit / drainEvents roundtrip")
        r = send(P,
            "thought.emit(7,'a stray thought','random'); "
            "local e=thought.drainEvents(); if #e<1 then return 'NONE' end; "
            "return e[1].target..'|'..e[1].kind..'|'..(e[1].payload.text or '?')")
        passed &= check("emit then drain returns the event",
                         r == "7|random|a stray thought", r)
        r2 = send(P, "return #thought.drainEvents()")
        passed &= check("second drain is empty", r2 == "0", r2)

        rep.note("2. data/thoughts.yaml loads")
        n = send(P, "return require('scripts.thoughts').loadCatalogue()")
        passed &= check("catalogue has entries", int(float(n)) >= 10, n)

        # A thought being ELIGIBLE doesn't mean it wins any given roll —
        # it competes in the same weighted pool as the always-eligible
        # "random" entries (that's by design: a cold environment doesn't
        # suppress ambient thoughts, it just adds to the pool). So poll a
        # few atomic rolls and accept the first one `matches`, rather
        # than asserting on a single roll.
        #
        # Each attempt drains the stream BEFORE forcing the tick and then
        # keeps only the event whose target is THIS unit, so a leftover
        # event from an earlier phase, or one another unit's background
        # tick queued in between, can never be misread as this roll's
        # outcome (the engine's thought stream is global, and every phase
        # plus unit_resources' own ticking share it).
        #
        # `mood`, when given, is re-applied inside each roll's own chunk:
        # a one-time assignment would not hold, because unit_resources'
        # background update calls brain.tick between console round trips
        # and drifts mood back toward its physiological target. A Lua
        # chunk can't be interleaved, so pinning it here means the roll
        # that follows sees exactly this value.
        #
        # Returns (hit, samples): `hit` is the matching outcome or None,
        # and `samples` is EVERY attempt's outcome in order — including
        # the 'NONE' of a roll that fired nothing for this unit — so an
        # exhausted poll can report the whole sample set instead of only
        # the last attempt.
        def roll_until(uid, matches, attempts=20, mood=None):
            pin = "" if mood is None else f"unit.setStat({uid},'mood',{mood}); "
            samples = []
            for _ in range(attempts):
                r = send(P,
                    f"thought.drainEvents(); "
                    f"unit.setStat({uid},'thought_next_at',0); "
                    f"{pin}"
                    f"require('scripts.thoughts').tick({uid}, unit.getInfo({uid}), 0.2); "
                    f"local e=thought.drainEvents(); "
                    f"for _,ev in ipairs(e) do if ev.target=={uid} then "
                    f"return ev.kind..'|'..ev.payload.text end end; "
                    f"return 'NONE'")
                samples.append(r)
                if matches(r):
                    return r, samples
            return None, samples

        def report_samples(samples):
            rep.note(f"  polled {len(samples)} roll(s), every outcome in order:")
            for i, s in enumerate(samples, 1):
                rep.note(f"    {i:3d}. {s}")

        rep.note("3. STATE thought: high pain -> 'state' category + mood moves")
        uid = int(float(send(P, "local u=unit.spawn('acolyte',1,0); return u")))
        time.sleep(0.8)
        # Fraction, not raw severity: painFrac = getPain()/PAIN_CEILING(5.0),
        # so pain_high's >0.5 threshold needs several wounds, not one hit.
        for part in ("l_forearm", "r_forearm", "l_thigh", "r_thigh", "torso"):
            send(P, f"return unit.injure({uid},'{part}','slash',0.9,0.0)")
        moodBefore = float(send(P, f"return require('scripts.brain').mood({uid})"))
        # Pass criterion unchanged (the category is the claim here); only
        # the failure REPORTING improves, and no mood is pinned — this
        # phase asserts that mood MOVES.
        hit, samples = roll_until(uid, lambda r: r.startswith("state|"))
        moodAfter = float(send(P, f"return require('scripts.brain').mood({uid})"))
        rep.note(f"  fired: {hit or 'NONE'}  mood {moodBefore:.4f} -> {moodAfter:.4f}")
        if hit is None:
            report_samples(samples)
        passed &= check("fired a 'state' thought", hit is not None,
                        hit or f"{len(samples)} rolls, none in the 'state' category")
        passed &= check("mood measurably moved", abs(moodAfter - moodBefore) > 0.005,
                         f"{moodBefore:.4f} -> {moodAfter:.4f}")

        rep.note("4. ENVIRONMENTAL thought: arctic ambient -> the COLD thought")
        uid2 = int(float(send(P, "local u=unit.spawn('acolyte',3,0); return u")))
        time.sleep(0.8)
        # Establish the conditions that decide which environmental
        # entries compete, so the outcome rides neither the arena's
        # incidental time of day nor the unit's live mood (see the
        # COLD_AMBIENT_C / NOON_SUN_ANGLE / PHASE4_MOOD notes above).
        # Nothing here edits the catalogue, a trigger, a weight, a
        # valence or the mood-bias curve: the cold thought still wins by
        # ordinary weighted selection or not at all.
        send(P, "_ORIG_AMBIENT = world.getAmbientAt; "
                "_ORIG_SUNANGLE = world.getSunAngleAt; "
                f"world.getAmbientAt = function(gx,gy) return {COLD_AMBIENT_C} end; "
                f"world.getSunAngleAt = function(gx,gy) return {NOON_SUN_ANGLE} end; "
                "return 'ok'")
        try:
            hit, samples = roll_until(uid2, lambda r: r == COLD_THOUGHT,
                                      attempts=PHASE4_ATTEMPTS, mood=PHASE4_MOOD)
            # Read the preconditions back through the very predicates
            # selection uses, while the patches are still installed. The
            # sample list alone cannot separate "cold was never eligible"
            # from "cold was eligible and lost every roll" — identical
            # non-cold samples come out of both — so eligibility is
            # reported from the controlled inputs instead of inferred.
            ctx = send(P,
                f"local i=unit.getInfo({uid2}); "
                f"local th=require('scripts.thoughts'); "
                f"local a=world.getAmbientAt(i.gridX,i.gridY); "
                f"local s=world.getSunAngleAt(i.gridX,i.gridY); "
                f"return 'ambient='..tostring(a)..' sunAngle='..tostring(s)"
                f"..' coldEligible='..tostring(th.TRIGGERS.cold({{ambient=a}}))"
                f"..' dayEligible='..tostring(th.TRIGGERS.day({{sunAngle=s}}))"
                f"..' nightEligible='..tostring(th.TRIGGERS.night({{sunAngle=s}}))")
        finally:
            # Restore before phase 5, which reuses this same Lua process.
            send(P, "world.getAmbientAt = _ORIG_AMBIENT; "
                    "world.getSunAngleAt = _ORIG_SUNANGLE; return 'ok'")
        rep.note(f"  fired: {hit or 'NONE'}")
        rep.note(f"  preconditions: pinnedMood={PHASE4_MOOD} {ctx}")
        if hit is None:
            report_samples(samples)
        passed &= check("fired the COLD environmental thought", hit is not None,
                        hit or f"{len(samples)} rolls, none cold; {ctx}")
        r = send(P, "return tostring(world.getAmbientAt == _ORIG_AMBIENT)"
                    "..'|'..tostring(world.getSunAngleAt == _ORIG_SUNANGLE)")
        passed &= check("phase 4's world patches are restored", r == "true|true", r)

        rep.note("5. state of mind biases selection (mood-weighted valence)")
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
        rep.note(f"  negative-pick fraction: mood=0.05 -> {frac_low:.2f}, "
                 f"mood=0.95 -> {frac_high:.2f}")
        passed &= check("low mood draws negative valence far more than high mood",
                         frac_low - frac_high > 0.3,
                         f"low={frac_low:.2f} high={frac_high:.2f}")

        rep.note("6. thought_log.lua data path: emit -> update() -> unitEntries()")
        send(P, "engine.loadScript('scripts/thought_log.lua', 0.1); return 'ok'")
        time.sleep(0.3)
        send(P, f"thought.emit({uid3},'PROBE_CHECK_TEXT','random'); return 'ok'")
        send(P, "require('scripts.thought_log').update(0.1); return 'ok'")
        r = send(P,
            f"local es=require('scripts.thought_log').unitEntries({uid3}) "
            f"if #es<1 then return 'NONE' end return es[1].text")
        passed &= check("thought_log surfaces the emitted text",
                         "PROBE_CHECK_TEXT" in r, r)

        rep.note(f"\n{'PASS' if passed else 'FAIL'} — thought system (#351)")
        return 0 if passed else 1
    finally:
        quit_engine(P, proc)


if __name__ == "__main__":
    sys.exit(main())
