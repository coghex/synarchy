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

This probe implements the shared `probe-result/v1` contract: `--describe`
prints its ordered stable checks without booting an engine, and a harnessed
run writes structured events while a standalone run keeps human-readable
per-check output.

Usage: python3 tools/concussion_revive_probe.py [--port 9304]
       python3 tools/concussion_revive_probe.py --describe
Exit 0 = pass.
"""
from __future__ import annotations
import argparse
import json
import sys
import time

sys.path.insert(0, "tools")
import probe_protocol
from probelib import quit_engine, boot, send
from collapse_crawl_probe import bootstrap

RISE_AT = 0.40
LOG_NAME = "concussion_revive_probe_engine.log"
PROBE_KEY = "concussion_revive"

CHECKS = [
    ("in_band_stays_collapsed",
     "in-band concussion keeps a conscious unit collapsed"),
    ("below_band_rises",
     "below-band concussion lets a conscious unit rise to standing"),
]

DESCRIPTOR = probe_protocol.build_descriptor(PROBE_KEY, CHECKS)


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


def run_case(P, idx, concussion, expect, check_id, rep):
    """Spawn a fresh unit, stamp a concussion, collapse it via consciousness,
    then restore consciousness and watch the pose. `expect` is "collapsed"
    (in-band → stays down) or "standing" (below band → rises)."""
    uid = int(float(send(P, f"local u=unit.spawn('acolyte',{idx},2); return u")))
    send(P, f"return unit.injure({uid},'head','concussion',{concussion},0.0)")
    conc = float(snap(P, uid).get("conc", 0.0))
    if not collapse_via_consciousness(P, uid):
        return rep.check(
            check_id, False,
            f"case conc={concussion}: unit never collapsed",
            {"requested_concussion": concussion,
             "observed_concussion": conc,
             "collapsed": False})
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
        return rep.check(
            check_id, False,
            f"case conc={conc:.2f}: consciousness never rose above "
            f"{RISE_AT} (max {c_hi:.2f}) — test invalid",
            {"requested_concussion": concussion,
             "observed_concussion": conc,
             "max_consciousness": c_hi,
             "poses": sorted({str(p) for p in poses}),
             "settled_pose": settled})

    if expect == "collapsed":
        # In-band concussion: must NEVER leave collapse.
        left = [p for p in poses if p not in ("collapsed",)]
        ok = not left
        observed_poses = sorted({str(p) for p in poses})
        return rep.check(
            check_id, ok,
            f"concussion {conc:.2f} in band (0.25..0.35): stays collapsed "
            f"while conscious (c≤{c_hi:.2f}); poses={observed_poses}",
            {"requested_concussion": concussion,
             "observed_concussion": conc,
             "max_consciousness": c_hi,
             "poses": observed_poses,
             "settled_pose": settled})
    else:
        # Below band: must rise (stand — no legs broken).
        ok = settled == "standing"
        return rep.check(
            check_id, ok,
            f"concussion {conc:.2f} below band: rises to standing once "
            f"conscious; settled={settled}",
            {"requested_concussion": concussion,
             "observed_concussion": conc,
             "max_consciousness": c_hi,
             "poses": sorted({str(p) for p in poses}),
             "settled_pose": settled})


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9304)
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


def _run(port, rep):
    # `boot` historically used its per-port fallback for this probe; retain
    # that standalone path while giving every harnessed run an isolated log.
    fallback_log = f"/tmp/synarchy_probe_{port}.log"
    proc = boot(port, log=rep.engine_log_path(LOG_NAME, fallback_log),
                args=rep.engine_args())
    try:
        bootstrap(port)
        ok = True
        ok &= run_case(port, 1, 0.30, "collapsed",
                       "in_band_stays_collapsed", rep)
        ok &= run_case(port, 5, 0.20, "standing",
                       "below_band_rises", rep)
        rep.note(f"\n{'PASS' if ok else 'FAIL'} — concussion rise-band "
                 "hysteresis (#304)")
        return 0 if ok else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
