#!/usr/bin/env python3
"""Concussion rise-band hysteresis probe (#304, checkRevive path).

Companion to collapse_crawl_probe.py. That probe exercises the collapse↔crawl
branch in tickInjuries; THIS one exercises the OTHER collapsed-state exit —
checkRevive (Collapsed→Standing) — for the CONCUSSION axis.

The bug: checkRevive let a collapsed unit stand up as soon as its concussion
dropped below the OUT threshold (0.35) — but a collapsed unit should stay down
until concussion clears the lower CONCUSSION_RISE band (0.25), the same
hysteresis the rest of #304 uses. Otherwise a concussion healing through
0.25..0.35 flaps the pose.

How it's tested deterministically: concussion severity is exact at stamp time
(effective = inflicted while heal=0), and consciousness is driven via
blood_oxygen in a temperate arena. We stamp a concussion, collapse the unit by
dropping consciousness (NOT via the concussion), then raise consciousness back
above RISE_AT. Now the ONLY thing that can keep the unit down is the concussion
gate:

  * concussion 0.30 (INSIDE the rise band) → must STAY collapsed (the fix;
    before the fix checkRevive would stand it up).
  * concussion 0.20 (BELOW the rise band) → must STAND up (proves it's gated
    by the band, not just stuck).

No leg damage, so the only down-keeping injury is the concussion.

Usage: python3 tools/concussion_revive_probe.py [--port 9304]
Exit 0 = pass.
"""
from __future__ import annotations
import argparse, json, socket, sys, time

sys.path.insert(0, "tools")
from probelib import quit_engine, boot, send
from collapse_crawl_probe import bootstrap

RISE_AT = 0.40


def snap(P, uid):
    raw = send(P, f"local u={uid} local b=require('scripts.brain') "
                  f"local i=require('scripts.injuries') "
                  f"return {{pose=unit.getPose(u) or 'nil', c=b.consciousness(u), "
                  f"conc=i.concussionSeverity(u)}}")
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return {"_raw": raw}


def collapse_via_consciousness(P, uid):
    """Drop blood_oxygen to 0 until the unit collapses (low consciousness,
    NOT the concussion)."""
    for _ in range(20):
        send(P, f"unit.setStat({uid},'blood_oxygen',0.0); return 'ok'")
        time.sleep(0.1)
        if snap(P, uid).get("pose") == "collapsed":
            return True
    return False


def run_case(P, idx, concussion, expect):
    """Spawn a fresh unit, stamp a concussion, collapse it via consciousness,
    then restore consciousness and watch the pose. `expect` is "collapsed"
    (in-band → stays down) or "standing" (below band → rises)."""
    uid = int(float(send(P, f"local u=unit.spawn('acolyte',{idx},2); return u")))
    send(P, f"return unit.injure({uid},'head','concussion',{concussion},0.0)")
    conc = float(snap(P, uid).get("conc", 0.0))
    if not collapse_via_consciousness(P, uid):
        print(f"  [FAIL] case conc={concussion}: unit never collapsed")
        return False
    # Restore consciousness well above RISE_AT and hold it there; poll the pose.
    poses, cons = [], []
    for _ in range(16):
        send(P, f"unit.setStat({uid},'blood_oxygen',1.0); return 'ok'")
        s = snap(P, uid)
        poses.append(s.get("pose"))
        cons.append(float(s.get("c", 0.0)))
        time.sleep(0.3)

    settled = poses[-1]
    c_hi = max(cons)
    # Validity: consciousness must have risen above the gate, so the concussion
    # is the only thing that could keep it down.
    if c_hi < RISE_AT:
        print(f"  [FAIL] case conc={conc:.2f}: consciousness never rose above "
              f"{RISE_AT} (max {c_hi:.2f}) — test invalid")
        return False

    if expect == "collapsed":
        # In-band concussion: must NEVER leave collapse.
        left = [p for p in poses if p not in ("collapsed",)]
        ok = not left
        print(f"  [{'pass' if ok else 'FAIL'}] concussion {conc:.2f} in band "
              f"(0.25..0.35): stays collapsed while conscious "
              f"(c≤{c_hi:.2f}); poses={sorted(set(poses))}")
        return ok
    else:
        # Below band: must rise (stand — no legs broken).
        ok = settled == "standing"
        print(f"  [{'pass' if ok else 'FAIL'}] concussion {conc:.2f} below band: "
              f"rises to standing once conscious; settled={settled}")
        return ok


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9304)
    args = ap.parse_args()
    P = args.port
    proc = boot(P)
    try:
        bootstrap(P)
        ok = True
        ok &= run_case(P, 1, 0.30, "collapsed")   # in band → stays down (the fix)
        ok &= run_case(P, 5, 0.20, "standing")     # below band → rises
        print(f"\n{'PASS' if ok else 'FAIL'} — concussion rise-band hysteresis (#304)")
        return 0 if ok else 1
    finally:
        quit_engine(P, proc)


if __name__ == "__main__":
    sys.exit(main())
