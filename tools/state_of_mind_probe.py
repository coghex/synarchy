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
  4. THE REGRESSION GUARD this issue calls for: the unit stays standing
     and brain.isUnconscious/isDelirious/isConfused all stay false — the
     locomotor collapse machine and the AI's delirium gate key on
     consciousness ALONE, unaffected by the psychological layer.
     (physiological collapse itself is separately covered end-to-end by
     tools/collapse_crawl_probe.py and tools/concussion_revive_probe.py,
     which this change must also keep passing.)

     Sampling that guard only after a single injury cannot detect the
     regression it names (#1761): substituting state_of_mind for
     consciousness changes an answer ONLY when the two land on opposite
     sides of one of brain.lua's three thresholds, and one slash leaves
     both comfortably above all three. So the guard now runs three
     purpose-built fixtures that put state_of_mind squarely inside each
     production band — [0.40,0.70), [0.15,0.40), [0,0.15) — while
     consciousness is held at the top of its own. Each fixture reaches
     that separation the way production does: consciousness stays high
     through its OWN three inputs (core_temp / blood_oxygen /
     salt_conc — pain is not one of them), and the depression is
     written into mood and emotional_pain, which brain.tick blends
     through computeStateOfMind. All three fixtures run even when an
     earlier one fails, so a mutated brain.state exposes the wrong
     answer in every band rather than only the first.
  5. The awareness/perception input (#350's "read from existing systems"
     list): brain.awareness(uid) reads the 'perception' stat instantly
     (no drift — same normalization Unit.LineOfSight uses), and an
     otherwise-healthy unit with suppressed perception alone (no pain,
     no hunger/stamina deficit) still sees its mood target dragged down
     by the awareness term over a few ticks. Uses bear_brown (no
     equipment) rather than the acolyte: acolytes spawn wearing
     technogoggles (+perception buff, data/items/technogoggles.yaml),
     and unit.setStat only overwrites the BASE stat — getStat still
     returns base + active modifiers — so an equipped unit's effective
     perception wouldn't land at the raw value this test writes.

Usage: python3 tools/state_of_mind_probe.py [--port 9350]
       python3 tools/state_of_mind_probe.py --describe
Exit 0 = pass.

This probe implements the shared `probe-result/v1` contract: `--describe`
prints its ordered stable checks without booting an engine, and a harnessed
run writes structured events while a standalone run keeps human-readable
per-check output.
"""
from __future__ import annotations
import argparse, glob, json, socket, subprocess, sys, time

import probe_protocol
from probelib import quit_engine, boot, send

LOG = "/tmp/state_of_mind_probe_engine.log"
LOG_NAME = "state_of_mind_probe_engine.log"
PROBE_KEY = "state_of_mind"
PAIN_CEILING = 5.0  # brain.lua PAIN_CEILING — keep in lockstep

CHECKS = [
    ("fresh_mental_baseline", "a fresh unit begins alert and content"),
    ("hungerless_species_fallback",
     "a species without hunger or hydration config computes mental values"),
    ("pain_concentration_response", "physical pain immediately reduces concentration"),
    ("pain_emotional_response", "emotional pain rises toward sustained physical pain"),
    ("pain_mood_response", "mood declines under sustained physical pain"),
    ("pain_gate_isolation", "pain-driven psychology does not trip physiological gates"),
    ("confused_band_gate_isolation",
     "a confused-band state of mind does not trip consciousness gates"),
    ("delirious_band_gate_isolation",
     "a delirious-band state of mind does not trip consciousness gates"),
    ("unconscious_band_gate_isolation",
     "an unconscious-band state of mind does not trip consciousness gates"),
    ("awareness_perception_response", "awareness reflects the live perception stat"),
    ("awareness_mood_response", "low awareness reduces mood over time"),
]

DESCRIPTOR = probe_protocol.build_descriptor(PROBE_KEY, CHECKS)

# brain.lua's consciousness bands — keep in lockstep. The guard below reads
# them as the boundaries of the three DISJOINT production intervals that
# brain.state / isConfused / isDelirious / isUnconscious cut consciousness
# into: [CONFUSED_BELOW,1] alert, [DELIRIOUS_BELOW,CONFUSED_BELOW) confused,
# [UNCONSCIOUS_BELOW,DELIRIOUS_BELOW) delirious, [0,UNCONSCIOUS_BELOW) out.
CONFUSED_BELOW    = 0.70
DELIRIOUS_BELOW   = 0.40
UNCONSCIOUS_BELOW = 0.15

# The three regression-guard fixtures (#1761). Each drives state_of_mind
# into ONE of the non-alert bands while consciousness stays >= 0.70, so a
# predicate that read state_of_mind instead of consciousness would return
# the `wrong_state` named here instead of "alert"/false.
#
# Targets are `mood - 0.5 * emotional_pain` (brain.lua's computeStateOfMind
# wellbeing term, which state_of_mind then min()s against consciousness),
# picked mid-band so the slow drift back toward a healthy mood target
# between the write and the sample cannot walk a sample out of its band:
#   0.75 - 0.5*0.40 = 0.55   0.57 - 0.5*0.60 = 0.27   0.46 - 0.5*0.80 = 0.06
GUARD_BANDS = [
    # label,         mood, emotional_pain, band low,          band high,         wrong state
    ("confused",     0.75, 0.40,           DELIRIOUS_BELOW,   CONFUSED_BELOW,    "confused"),
    ("delirious",    0.57, 0.60,           UNCONSCIOUS_BELOW, DELIRIOUS_BELOW,   "delirious"),
    ("unconscious",  0.46, 0.80,           0.0,               UNCONSCIOUS_BELOW, "unconscious"),
]

# The "has production run yet?" marker. computeStateOfMind min()s a 0..1
# wellbeing against a 0..1 consciousness, so it can never produce a value
# above 1.0 — a stored state_of_mind that has dropped back into range can
# only be a real brain.tick recomputation, never the probe's own write.
#
# It must be ABOVE the range, not below: unit.setStat clamps its argument at
# >= 0 (`clamped = max 0 v`, src/Engine/Scripting/Lua/API/Units/Stats.hs),
# so a negative marker would silently store 0.0 and read as an already-valid
# value, making the wait vacuous. There is no upper clamp. band_fixture
# re-reads the marker inside the same Lua chunk that wrote it and fails loudly
# if it did not survive, so a future clamp change cannot quietly restore that
# vacuum.
RECOMPUTE_SENTINEL = 2.0


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
    # Neutralise periodic thoughts (#351): each unit rolls its first thought
    # at a random 0-30 game-second deadline, and a fired thought overwrites
    # 'mood' directly (scripts/thoughts.lua) — able to land inside this
    # probe's ~4s sampling windows and mask (or fake) the pain/awareness
    # mood drift under test (#793). unit_resources.lua's shared thoughts
    # table (required above via unit_resources.lua) is the same object this
    # require() returns, so the no-op reaches the real tick call; brain.tick
    # and the rest of the physiology chain stay untouched.
    send(port, "pcall(function() require('scripts.thoughts').tick = "
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


def gate_observation(port, uid):
    """One Lua-side read of everything the guard asserts on.

    Lua is single-threaded and the physiology tick runs on that same
    thread, so no tick can interleave inside this chunk: consciousness,
    state_of_mind, the pose and all four consciousness-keyed answers
    below are necessarily the ones produced by the SAME brain.tick.
    Reading them as separate console round trips (as this guard used to)
    lets a tick land between the two halves, which is exactly what the
    acceptance condition's "moment the gates were read" rules out.
    """
    return json.loads(send(port,
        f"local u={uid} local b=require('scripts.brain') "
        f"return {{c=b.consciousness(u), som=b.stateOfMind(u), "
        f"mood=b.mood(u), ep=b.emotionalPain(u), pose=unit.getPose(u), "
        f"uncon=b.isUnconscious(u), delir=b.isDelirious(u), "
        f"conf=b.isConfused(u), state=b.state(u)}}"))


def gate_text(g):
    return (f"pose={g['pose']} uncon={g['uncon']} delir={g['delir']} "
            f"conf={g['conf']} state={g['state']}")


def gates_alert(g):
    """The guard's assertion, unchanged in substance since #350."""
    return (g["pose"] == "standing" and not g["uncon"] and not g["delir"]
            and not g["conf"] and g["state"] == "alert")


def stored_state_of_mind(port, uid):
    """The raw stored stat, or None if it did not read back as a number."""
    try:
        return float(send(port, f"return unit.getStat({uid},'state_of_mind')"))
    except (TypeError, ValueError):
        return None


def band_fixture(port, uid, mood, emotional_pain, timeout=8.0):
    """Depress state_of_mind into one band, leaving consciousness alone.

    Consciousness is re-affirmed through its own three inputs rather
    than written directly, and the depression goes in through mood and
    emotional_pain — the pair brain.tick actually blends — so the value
    the guard samples is production's own, not one the next tick would
    recompute away.

    The out-of-range marker is what makes that last claim checkable: the
    write and its read-back share ONE Lua chunk, so no tick can slip
    between them and the read-back proves the marker really landed; the
    wait then proves a real brain.tick replaced it before anything is
    sampled. Returns ``(observation, None)``, or ``(None, reason)``.
    """
    landed = send(port, f"local u={uid} "
                        f"unit.setStat(u,'core_temp',37.0) "
                        f"unit.setStat(u,'blood_oxygen',1.0) "
                        f"unit.setStat(u,'salt_conc',1.0) "
                        f"unit.setStat(u,'mood',{mood}) "
                        f"unit.setStat(u,'emotional_pain',{emotional_pain}) "
                        f"unit.setStat(u,'state_of_mind',{RECOMPUTE_SENTINEL}) "
                        f"return unit.getStat(u,'state_of_mind')")
    try:
        landed_val = float(landed)
    except (TypeError, ValueError):
        return None, (f"the recompute marker did not read back as a number "
                      f"({landed!r}) — the wait below would prove nothing")
    if landed_val <= 1.0:
        return None, (f"unit.setStat stored {landed_val} for the "
                      f"{RECOMPUTE_SENTINEL} recompute marker, which is inside "
                      f"the producible 0..1 range — the wait below would pass "
                      f"before brain.tick ever ran and prove nothing")
    deadline = time.time() + timeout
    while time.time() < deadline:
        time.sleep(0.1)
        current = stored_state_of_mind(port, uid)
        if current is not None and current <= 1.0:
            return gate_observation(port, uid), None
    return None, ("brain.tick never replaced the recompute marker, so no "
                  "production-computed state_of_mind was ever available")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9350)
    ap.add_argument("--describe", action="store_true",
                    help="print the probe-result/v1 check declaration and "
                         "exit without booting an engine")
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
    proc = boot(P, log=rep.engine_log_path(LOG_NAME, LOG),
                args=rep.engine_args())
    ok = True
    try:
        bootstrap(P)

        # ---- 1. Fresh unit reads as fully alert/content. ----
        uid = send(P, "local u=unit.spawn('acolyte',1,0); return u")
        uid = int(float(uid))
        rep.note(f"spawned acolyte uid={uid}")
        s0 = summary(P, uid)
        rep.note(f"  fresh: {s0}")
        fresh_ok = (s0.get("state") == "alert"
                    and close(s0.get("consciousness", 0), 1.0)
                    and close(s0.get("mood", 0), 1.0)
                    and close(s0.get("emotionalPain", 1), 0.0)
                    and close(s0.get("concentration", 0), 1.0)
                    and close(s0.get("stateOfMind", 0), 1.0))
        ok &= rep.check(
            "fresh_mental_baseline", fresh_ok,
            ("fresh unit: alert, mood/concentration/stateOfMind ~1.0, no "
             "emotional pain" if fresh_ok else
             f"fresh unit not at baseline: {s0}"),
            {"summary": s0})

        # ---- 3. Species with no hunger/hydration config computes cleanly. ----
        bear = send(P, "local u=unit.spawn('bear_brown',5,0); return u")
        bear = int(float(bear))
        sb = summary(P, bear)
        rep.note(f"  bear_brown (no hunger config) uid={bear}: {sb}")
        bear_ok = (sb.get("state") == "alert"
                   and all(isinstance(sb.get(k), (int, float))
                           for k in ("consciousness", "mood", "emotionalPain",
                                     "concentration", "stateOfMind")))
        ok &= rep.check(
            "hungerless_species_fallback", bear_ok,
            ("hunger-less species computes all mental values (fallback, no "
             "crash)" if bear_ok else
             f"bear_brown summary malformed: {sb}"),
            {"unit": "bear_brown", "summary": sb})

        # ---- 2. Physical pain: instantaneous concentration hit + drift. ----
        send(P, f"return unit.injure({uid},'l_forearm','slash',0.6,0.0)")
        rawpain = send(P, f"return unit.getPain({uid})")
        pain_raw = float(rawpain)
        pain_frac = max(0.0, min(1.0, pain_raw / PAIN_CEILING))
        time.sleep(0.5)
        s1 = summary(P, uid)
        expected_conc = 1.0 * (1.0 - 0.6 * pain_frac) * 1.0  # stamina still ~full
        rep.note(f"  injured (raw pain={pain_raw:.3f}, "
                 f"frac={pain_frac:.3f}): {s1}")
        concentration_ok = close(s1.get("concentration", 0), expected_conc,
                                 tol=0.05)
        ok &= rep.check(
            "pain_concentration_response", concentration_ok,
            (f"concentration drops to ~{expected_conc:.3f} immediately on "
             "injury" if concentration_ok else
             f"concentration {s1.get('concentration')} != expected "
             f"~{expected_conc:.3f}"),
            {"concentration": s1.get("concentration"),
             "expected_concentration": expected_conc,
             "pain_raw": pain_raw, "pain_fraction": pain_frac})

        # Let pain persist a few seconds — emotional_pain should climb toward
        # pain_frac (fast rise), mood should drop below its fresh baseline.
        samples = []
        for _ in range(8):
            time.sleep(0.5)
            samples.append(summary(P, uid))
        ep_trend = [s.get("emotionalPain", 0) for s in samples]
        mood_trend = [s.get("mood", 1) for s in samples]
        rep.note(f"  emotionalPain trend: {[round(x,3) for x in ep_trend]}")
        rep.note(f"  mood trend:          {[round(x,3) for x in mood_trend]}")

        rising = ep_trend[-1] > ep_trend[0] and ep_trend[-1] <= pain_frac + 0.05
        ok &= rep.check(
            "pain_emotional_response", rising,
            (f"emotional_pain rises toward pain fraction ({pain_frac:.3f}) "
             "over sustained pain" if rising else
             f"emotional_pain didn't rise sensibly: {ep_trend}"),
            {"trend": ep_trend, "pain_fraction": pain_frac})

        mood_down = mood_trend[-1] < mood_trend[0] - 0.002
        ok &= rep.check(
            "pain_mood_response", mood_down,
            (f"mood drifts down under sustained pain ({mood_trend[0]:.4f} "
             f"→ {mood_trend[-1]:.4f})" if mood_down else
             f"mood didn't drift down under sustained pain: {mood_trend}"),
            {"trend": mood_trend})

        # ---- 4. Regression guard: psychological tanking must NOT trip the
        # physiological collapse/delirium gates — those stay keyed on
        # consciousness alone.
        #
        # 4a. The injured unit from phase 2, as continuity: pain has moved
        # the psychological layer, and none of the gates moved with it.
        # This sample alone does NOT discriminate — both values are still
        # above every threshold, so a substituted predicate would answer
        # identically (#1761). The three band fixtures in 4b are what makes
        # the substitution observable.
        gate = gate_observation(P, uid)
        rep.note(f"  post-pain gate check: consciousness={gate['c']:.3f} "
                 f"state_of_mind={gate['som']:.3f} {gate_text(gate)}")
        pain_gate_ok = gates_alert(gate) and gate["som"] < gate["c"]
        ok &= rep.check(
            "pain_gate_isolation", pain_gate_ok,
            ("pain depressed the psychological layer "
             f"({gate['som']:.3f} < {gate['c']:.3f}) without moving pose or "
             "any gate" if pain_gate_ok else
             f"post-pain gate check: {gate_text(gate)}"),
            gate)

        # 4b. THE DISCRIMINATING GUARD (#1761): one fixture per production
        # band. Consciousness is held at the top of its own band through
        # core_temp / blood_oxygen / salt_conc, while mood and emotional
        # pain drag state_of_mind below 0.70, then 0.40, then 0.15. In each
        # one, reading state_of_mind where the code must read consciousness
        # would answer "confused" / "delirious" / "unconscious" and flip the
        # matching predicate — so the assertion below (unchanged in
        # substance: standing, all three predicates false, state "alert")
        # can now actually fail for the reason it names. Every band runs
        # even after an earlier one fails, so a mutation is exposed in all
        # three rather than only the first.
        guard_uid = int(float(send(P, "local u=unit.spawn('acolyte',13,0); return u")))
        rep.note(f"  --- regression-guard band fixtures, uid={guard_uid} ---")
        for label, mood, ep, low, high, wrong in GUARD_BANDS:
            g, why = band_fixture(P, guard_uid, mood, ep)
            if g is None:
                human = (f"{label} band (mood={mood}, "
                         f"emotional_pain={ep}): {why}")
                detail = {"band": label, "mood": mood,
                          "emotional_pain": ep, "error": why}
                ok &= rep.check(f"{label}_band_gate_isolation", False,
                                human, detail)
                continue
            rep.note(f"  {label} band: consciousness={g['c']:.3f} "
                     f"state_of_mind={g['som']:.3f} "
                     f"(want [{low:.2f},{high:.2f}) with consciousness >= "
                     f"{CONFUSED_BELOW:.2f}) mood={g['mood']:.3f} "
                     f"emotional_pain={g['ep']:.3f} {gate_text(g)}")
            separated = (low <= g["som"] < high
                         and g["c"] >= CONFUSED_BELOW)
            check_ok = separated and gates_alert(g)
            if not separated:
                human = (f"{label} band fixture did not separate the two "
                         f"values — state_of_mind {g['som']:.3f} not in "
                         f"[{low:.2f},{high:.2f}) or consciousness "
                         f"{g['c']:.3f} < {CONFUSED_BELOW:.2f}; the guard "
                         "would prove nothing at this sample")
            elif check_ok:
                human = (f"REGRESSION GUARD ({label} band): state_of_mind "
                         f"{g['som']:.3f} < {high:.2f} <= consciousness "
                         f"{g['c']:.3f}, and the consciousness-keyed gates "
                         f"all still read alert — reading state_of_mind here "
                         f"would have said \"{wrong}\"")
            else:
                human = ("psychological layer leaked into physiological "
                         f"gating in the {label} band: consciousness="
                         f"{g['c']:.3f} state_of_mind={g['som']:.3f} "
                         f"{gate_text(g)} (expected standing/false/false/"
                         f"false/alert; \"{wrong}\" is the state_of_mind "
                         "answer)")
            detail = dict(g)
            detail.update({"band": label, "band_low": low,
                           "band_high": high, "wrong_state": wrong,
                           "separated": separated})
            ok &= rep.check(f"{label}_band_gate_isolation", check_ok,
                            human, detail)

        # ---- 5. Awareness/perception input, isolated from every other
        # driver: a fresh, gear-free unit (no pain, full hunger/stamina)
        # with its perception stat suppressed. bear_brown carries no
        # equipment (no perception-buffing accessory to confound the
        # read), so setStat's base overwrite lands exactly on getStat's
        # effective value. brain.awareness() should reflect the drop
        # immediately (no tick needed — it's a live read), and mood should
        # drift down over a few ticks purely from the awareness term.
        aware_uid = int(float(send(P, "local u=unit.spawn('bear_brown',9,0); return u")))
        send(P, f"unit.setStat({aware_uid},'perception',0.2); return 'ok'")
        aware_now = float(send(P,
            f"return require('scripts.brain').awareness({aware_uid})"))
        rep.note(f"  awareness uid={aware_uid}: perception=0.2 -> "
                 f"brain.awareness()={aware_now:.3f}")
        awareness_ok = close(aware_now, 0.2, tol=0.02)
        ok &= rep.check(
            "awareness_perception_response", awareness_ok,
            ("brain.awareness() reflects suppressed perception instantly"
             if awareness_ok else
             f"brain.awareness() {aware_now:.3f} != expected ~0.2"),
            {"awareness": aware_now, "expected": 0.2})

        aware_samples = []
        for _ in range(8):
            time.sleep(0.5)
            aware_samples.append(summary(P, aware_uid))
        aware_mood_trend = [s.get("mood", 1) for s in aware_samples]
        rep.note(f"  low-awareness mood trend: "
                 f"{[round(x,3) for x in aware_mood_trend]}")
        # Expected asymptote: 1 - MOOD_W_AWARENESS*(1-0.2) = 1 - 0.15*0.8 = 0.88.
        awareness_mood_ok = (aware_mood_trend[-1]
                             < aware_mood_trend[0] - 0.002)
        ok &= rep.check(
            "awareness_mood_response", awareness_mood_ok,
            (f"mood drifts down from suppressed perception ALONE "
             f"({aware_mood_trend[0]:.4f} → {aware_mood_trend[-1]:.4f}, "
             "no pain/hunger/exhaustion)" if awareness_mood_ok else
             f"mood didn't respond to the awareness input: "
             f"{aware_mood_trend}"),
            {"trend": aware_mood_trend, "awareness": aware_now})

        rep.note(f"\n{'PASS' if ok else 'FAIL'} — unified state-of-mind model (#350)")
        return 0 if ok else 1
    finally:
        quit_engine(P, proc)


if __name__ == "__main__":
    sys.exit(main())
