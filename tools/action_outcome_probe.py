#!/usr/bin/env python3
"""F4 action-outcome oracle probe (#646).

Exercises the PUBLIC Lua contract end to end — the thing review round 1
on PR #704 flagged the hspec regression for bypassing (it swapped
`actionOutcomeRef` directly instead of going through the real
`debug.recordOutcome`/`debug.drainActionOutcomes` verbs). Boots a
headless engine and checks:

  1. debug.recordOutcome requires kind+outcome: a call missing either
     returns false and pushes nothing.
  2. A full record round-trips through debug.drainActionOutcomes with
     every field intact (ts/kind/outcome/where/target/requested/
     applied/dropped/reason/handler).
  3. The ring drains destructively: a second drainActionOutcomes()
     immediately after the first returns empty (same contract as
     combat.drainEvents/injury.drainEvents).
  4. The REQUIRED partial path: designating a rectangle straddling both
     tillable ground and a fluid/sloped/flora tile on a real generated
     world reports outcome="partial" with dropped > 0 and
     requested == applied + dropped.
  5. A designation anchored on an unloaded tile reports "rejected" with
     applied == 0 and a reason.
  6. Starting-portal placement through the real player-facing path
     (#779/#1399): on a location-less arena the click is structurally
     remote, so it must record buildTool.remoteWarning/"presented" and
     spawn nothing while placement stays armed, and only the modal's own
     establishHere() may then record "confirmed" followed by
     buildTool.commitPlacement/"accepted" with no reason and exactly one
     new building id.

Usage: python3 tools/action_outcome_probe.py [--port 9179] [--seed 42]
       [--size 64] [--plates 3]
"""
import argparse
import glob
import json
import sys
import time

from probelib import boot, quit_engine, send

SPROOT = "/tmp"


def jget(port, lua, timeout=10.0):
    raw = send(port, lua, timeout)
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return raw.strip('"')


def bootstrap(port):
    for pattern, fn in [
        ("data/materials/*.yaml", "engine.loadMaterialYaml"),
        ("data/flora/*.yaml",     "engine.loadFloraYaml"),
        ("data/buildings/*.yaml", "engine.loadBuildingYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")


def find_mixed_box(port, span=6):
    """Scan for an anchor tile that's flat/dry/flora-free with at least
    one DIFFERENT tile (fluid, sloped, or flora-carrying) inside a 5x5
    box around it — guarantees till.designate's filter drops something
    without depending on any specific seed's geography beyond "typical
    generated terrain has some variety within 5 tiles of most points"."""
    for sx in range(-span * 8, span * 8 + 1, 3):
        for sy in range(-span * 8, span * 8 + 1, 3):
            slope = jget(port, f"return world.getSlopeAt({sx},{sy})")
            if slope != 0:
                continue
            fluid = jget(port, f"return world.getFluidAt({sx},{sy})")
            if isinstance(fluid, dict) and fluid.get("type"):
                continue
            flora = jget(port, f"return world.getFloraAt({sx},{sy})")
            if isinstance(flora, dict):
                continue
            # Candidate anchor is tillable. Check a 5x5 box around it for
            # at least one tile that ISN'T (mixed sweep -> partial).
            mixed = False
            for dx in range(-2, 3):
                for dy in range(-2, 3):
                    if dx == 0 and dy == 0:
                        continue
                    gx, gy = sx + dx, sy + dy
                    s2 = jget(port, f"return world.getSlopeAt({gx},{gy})")
                    f2 = jget(port, f"return world.getFluidAt({gx},{gy})")
                    fl2 = jget(port, f"return world.getFloraAt({gx},{gy})")
                    if s2 != 0 or (isinstance(f2, dict) and f2.get("type")) \
                       or isinstance(fl2, dict):
                        mixed = True
                        break
                if mixed:
                    break
            if mixed:
                return sx, sy
    return None


def find_chop_mixed_box(port, span=8):
    """Scan for a tile carrying flora, then confirm designating a 5x5 box
    around it reports a genuine partial (>=1 tree designated, >=1 tile
    dropped) — proves chop's requested/applied/dropped is based on the
    full swept-TILE count, not the flora-INSTANCE count (the exact
    5x5-one-tree miscount review round 1 found: a naive count of flora
    instances reported 1/1/0 accepted instead of 25/1/24 partial)."""
    for sx in range(-span * 8, span * 8 + 1, 4):
        for sy in range(-span * 8, span * 8 + 1, 4):
            flora = jget(port, f"return world.getFloraAt({sx},{sy})")
            if not isinstance(flora, dict):
                continue
            send(port, "return debug.drainActionOutcomes()")  # clear noise
            send(port, f"chop.designate('probe',{sx-2},{sy-2},{sx+2},{sy+2},"
                       f"'wood'); return 'ok'")
            drained = jget(port, "return debug.drainActionOutcomes()")
            rec = drained[0] if isinstance(drained, list) and drained else {}
            if (rec.get("kind") == "chop.designate"
                    and rec.get("outcome") == "partial"
                    and isinstance(rec.get("applied"), (int, float))
                    and rec["applied"] >= 1
                    and isinstance(rec.get("dropped"), (int, float))
                    and rec["dropped"] > 0):
                return sx, sy, rec
    return None


def fail_setup(message):
    """Report a PORTAL-FIXTURE setup failure (#1399 requirement 6).

    A fixture that could not be established leaves the placement
    contract UNVERIFIED, which is a different claim from the contract
    being violated — the distinct prefix is what lets a reader tell
    "the arena/tile/modal was never usable" apart from "the real
    confirmation path misbehaved". Both still fail the run; only the
    chop fixture has its own exit status (#1398)."""
    print(f"  [FAIL] (fixture setup) {message} — portal placement "
          f"contract UNVERIFIED")


def active_building_ids(port):
    """The ACTIVE page's live building ids as a SET.

    The confirmation contract is "exactly one building that did not
    exist before", which a count alone cannot state: a simultaneous
    retire and spawn leaves the count unchanged, and a count also
    can't say WHICH id is new."""
    ids = jget(port, "return building.getActiveIds()")
    if not isinstance(ids, list):
        return set()
    return {int(i) for i in ids
            if isinstance(i, (int, float)) and not isinstance(i, bool)}


def wait_for_one_new_building(port, before_ids, timeout=30.0, interval=0.25):
    """Poll until the active id set is exactly `before_ids` plus one new
    id; returns (new_ids, after_ids) as observed at the deadline.

    building.spawn only QUEUES a BuildingSpawn message
    (src/Engine/Scripting/Lua/API/Buildings/Spawn.hs), while
    building.getActiveIds reads the manager state published afterwards
    (Buildings/Query.hs) — so the spawn is asynchronous. Observed live
    while verifying #1399: 0 buildings immediately after establishHere
    returned, 1 roughly two seconds later. Sampling once would flake,
    and a fixed sleep would only relocate the flake, so this returns as
    soon as the set settles and otherwise spends its whole bounded
    budget."""
    deadline = time.time() + timeout
    after_ids = active_building_ids(port)
    while True:
        new_ids = after_ids - before_ids
        if len(new_ids) == 1 and after_ids == before_ids | new_ids:
            return new_ids, after_ids
        if time.time() >= deadline:
            return new_ids, after_ids
        time.sleep(interval)
        after_ids = active_building_ids(port)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9179)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/action_outcome_probe_engine.log")
    try:
        bootstrap(port)

        # --- 1/2/3: the public recordOutcome/drainActionOutcomes contract ---
        send(port, "return debug.drainActionOutcomes()")  # clear any startup noise

        bad = jget(port, 'return debug.recordOutcome{kind="x"}')  # no outcome
        ok1 = bad is False
        passed &= ok1
        print(f"  [{'PASS' if ok1 else 'FAIL'}] recordOutcome without "
              f"outcome returns false: {bad}")

        after_bad = jget(port, "return debug.drainActionOutcomes()")
        ok1b = after_bad == {} or after_bad == []
        passed &= ok1b
        print(f"  [{'PASS' if ok1b else 'FAIL'}] the rejected call pushed "
              f"nothing: {after_bad}")

        ok = jget(port, 'return debug.recordOutcome{kind="probe.verb", '
                        'outcome="accepted", where={x=3,y=4}, target=7, '
                        'requested=2, applied=2, dropped=0, '
                        'reason="r", handler="h"}')
        ok2 = ok is True
        passed &= ok2
        print(f"  [{'PASS' if ok2 else 'FAIL'}] full-field recordOutcome "
              f"returns true: {ok}")

        drained = jget(port, "return debug.drainActionOutcomes()")
        rec = drained[0] if isinstance(drained, list) and drained else {}
        ok3 = (rec.get("kind") == "probe.verb" and rec.get("outcome") == "accepted"
               and rec.get("where") == {"x": 3, "y": 4} and rec.get("target") == 7
               and rec.get("requested") == 2 and rec.get("applied") == 2
               and rec.get("dropped") == 0 and rec.get("reason") == "r"
               and rec.get("handler") == "h")
        passed &= ok3
        print(f"  [{'PASS' if ok3 else 'FAIL'}] drained record has every "
              f"field intact: {drained}")

        second = jget(port, "return debug.drainActionOutcomes()")
        ok4 = second == {} or second == []
        passed &= ok4
        print(f"  [{'PASS' if ok4 else 'FAIL'}] second drain is empty "
              f"(destructive read): {second}")

        # Fractional `where` coordinates (review round 9): Lua's
        # tointeger refuses any non-integral number, which previously
        # dropped the WHOLE `where` field rather than just truncating
        # it. debug.recordOutcome now reads where.x/y with tonumber, so
        # a fractional click position (Layer A screen-space clicks, or
        # the playtest harness's deliberately non-integral injections)
        # must round-trip exactly.
        ok_float = jget(port, 'return debug.recordOutcome{kind="float.where", '
                               'outcome="accepted", where={x=1.5,y=2.25}}')
        drained_float = jget(port, "return debug.drainActionOutcomes()")
        rec_float = (drained_float[0]
                     if isinstance(drained_float, list) and drained_float
                     else {})
        ok4d = bool(ok_float is True
                    and rec_float.get("kind") == "float.where"
                    and rec_float.get("where") == {"x": 1.5, "y": 2.25})
        passed &= ok4d
        print(f"  [{'PASS' if ok4d else 'FAIL'}] fractional where "
              f"coordinates round-trip intact: {drained_float}")

        # wire.place, rejected path: no active world exists yet at this
        # point in the script, so structure.place must refuse and
        # wire.place must propagate that (review round 7 — it previously
        # discarded structure.place's own result and always reported
        # "accepted"; round 8 asked for an automated regression, not just
        # a live check).
        send(port, 'engine.loadScript("scripts/wire.lua", 0.0); return "ok"')
        send(port, "return debug.drainActionOutcomes()")  # clear noise
        send(port, 'require("scripts.wire").place(0, 0); return "ok"')
        drained_wire_reject = jget(port, "return debug.drainActionOutcomes()")
        wire_reject_rec = (drained_wire_reject[0]
                            if isinstance(drained_wire_reject, list) and drained_wire_reject
                            else {})
        no_world_hasat = jget(port, 'return structure.hasAt(0, 0, "wire")')
        ok4b = bool(wire_reject_rec.get("kind") == "wire.place"
                    and wire_reject_rec.get("outcome") == "rejected"
                    and wire_reject_rec.get("reason")
                    and no_world_hasat is False)
        passed &= ok4b
        print(f"  [{'PASS' if ok4b else 'FAIL'}] wire.place with no active "
              f"world reports rejected and places nothing: "
              f"{drained_wire_reject}, hasAt={no_world_hasat}")

        # --- 4/5: the real till.designate partial + rejected paths ---
        send(port, f"world.init('probe', {args.seed}, {args.size}, "
                   f"{args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-8, -8, 8, 8)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)

        # wire.place, accepted path: with a real active world and a
        # loaded tile, structure.place should actually succeed this
        # time, and the outcome record must carry NO reason (review
        # round 7's `ok and nil or "..."` bug always attached a failure
        # reason even on success).
        send(port, "return debug.drainActionOutcomes()")  # clear noise
        send(port, 'require("scripts.wire").place(0, 0); return "ok"')
        drained_wire_accept = jget(port, "return debug.drainActionOutcomes()")
        wire_accept_rec = (drained_wire_accept[0]
                            if isinstance(drained_wire_accept, list) and drained_wire_accept
                            else {})
        placed_hasat = jget(port, 'return structure.hasAt(0, 0, "wire")')
        ok4c = bool(wire_accept_rec.get("kind") == "wire.place"
                    and wire_accept_rec.get("outcome") == "accepted"
                    and wire_accept_rec.get("reason") is None
                    and placed_hasat is True)
        passed &= ok4c
        print(f"  [{'PASS' if ok4c else 'FAIL'}] wire.place on a real "
              f"active world reports accepted with no reason and "
              f"actually places the wire: {drained_wire_accept}, "
              f"hasAt={placed_hasat}")

        box = find_mixed_box(port)
        if not box:
            # A missing fixture means the partial path went UNVERIFIED,
            # not that it passed — fail loudly rather than silently
            # skip (review round 2).
            passed = False
            print("  [FAIL] no mixed tillable/non-tillable 5x5 box found in "
                  "the loaded region (try another seed) — till partial path unverified")
        else:
            sx, sy = box
            send(port, "return debug.drainActionOutcomes()")  # clear noise
            send(port, f"till.designate('probe',{sx-2},{sy-2},{sx+2},{sy+2}); "
                       f"return 'ok'")
            drained2 = jget(port, "return debug.drainActionOutcomes()")
            rec2 = drained2[0] if isinstance(drained2, list) and drained2 else {}
            requested = rec2.get("requested")
            applied = rec2.get("applied")
            dropped = rec2.get("dropped")
            ok5 = (rec2.get("kind") == "till.designate"
                   and rec2.get("outcome") == "partial"
                   and isinstance(dropped, (int, float)) and dropped > 0
                   and isinstance(requested, (int, float))
                   and isinstance(applied, (int, float))
                   and requested == applied + dropped)
            passed &= ok5
            print(f"  [{'PASS' if ok5 else 'FAIL'}] mixed till sweep at "
                  f"({sx},{sy}) reports partial: {drained2}")

        # The exact 5x5-one-tree regression review round 1 flagged: chop's
        # requested must be the full swept-tile count (25), not the
        # flora-instance count (1) — a naive count previously reported
        # 1/1/0 accepted instead of 25/1/24 partial.
        chop_box = find_chop_mixed_box(port)
        if not chop_box:
            passed = False
            print("  [FAIL] no tree found in the loaded region to designate "
                  "a mixed chop box against (try another seed) — chop "
                  "partial path unverified")
        else:
            cx, cy, chop_rec = chop_box
            requested = chop_rec.get("requested")
            applied = chop_rec.get("applied")
            dropped = chop_rec.get("dropped")
            ok5b = bool(requested == 25 and requested == applied + dropped)
            passed &= ok5b
            print(f"  [{'PASS' if ok5b else 'FAIL'}] mixed chop sweep at "
                  f"({cx},{cy}) reports the full 5x5=25 tile count as "
                  f"requested: {chop_rec}")

        send(port, "return debug.drainActionOutcomes()")  # clear noise
        send(port, "till.designate('probe',5000000,5000000,5000005,5000005); "
                   "return 'ok'")
        drained3 = jget(port, "return debug.drainActionOutcomes()")
        rec3 = drained3[0] if isinstance(drained3, list) and drained3 else {}
        ok6 = bool(rec3.get("kind") == "till.designate"
                   and rec3.get("outcome") == "rejected"
                   and rec3.get("applied") == 0 and rec3.get("reason"))
        passed &= ok6
        print(f"  [{'PASS' if ok6 else 'FAIL'}] unloaded-anchor sweep reports "
              f"rejected: {drained3}")

        # Review round 7: all four designation handlers silently returned
        # `pure ()` (no F4 record at all) when the queued page doesn't
        # exist — a DIFFERENT failure than "page exists, sweep found
        # nothing" above. A public till.designate('missing_page', ...)
        # call must still drain a rejected record.
        send(port, "return debug.drainActionOutcomes()")  # clear noise
        send(port, "till.designate('missing_page',0,0,2,2); return 'ok'")
        drained4 = jget(port, "return debug.drainActionOutcomes()")
        rec4 = drained4[0] if isinstance(drained4, list) and drained4 else {}
        ok7 = bool(rec4.get("kind") == "till.designate"
                   and rec4.get("outcome") == "rejected" and rec4.get("reason"))
        passed &= ok7
        print(f"  [{'PASS' if ok7 else 'FAIL'}] till.designate against a "
              f"missing world page reports rejected: {drained4}")

        # Portal placement through the REMOTE-SETTLEMENT CONFIRMATION
        # (#779), driven end to end on the real player-facing path:
        # world.pickTile resolves the click exactly like wire_probe.py's
        # path-builder phase, buildTool.handleMouseDown routes it, and
        # the modal's own establishHere() completes it. Uses its own
        # flat arena rather than the generated 'probe' world above —
        # pickTile needs a guaranteed-flat, guaranteed-loaded tile under
        # the click, which generated terrain doesn't promise.
        #
        # An arena has no placed locations, and Building.Placement's
        # `isRemote (RemoteDistance Nothing) = True`
        # (src/Building/Placement.hs) makes that STRUCTURALLY remote, so
        # this click deterministically opens the warning instead of
        # spawning. Not seed-dependent — which is why the pre-#779
        # single-click assertion this block used to carry failed on a
        # correct build (#1399): scripts/build_tool.lua's isStarting
        # branch opens the warning, spawns nothing, and deliberately
        # leaves placement armed.
        #
        # The modal is prepared for headless use the same way bt.hud is
        # below. buildToolRemoteWarning.init is only ever called from
        # ui_manager_boot.lua, which never runs headless; without it
        # UI.newBox returns nil against a nil texture set, so open()
        # raises on `clickHandlers[nil] = onClick` BEFORE reaching its
        # own recordOutcome — no record at all, an empty drain, and a
        # "presented" assertion that could never pass. init(1,2,3,...)
        # is the same synthetic-handle technique the headless UI suite
        # already uses (test-headless/Test/Headless/UI/
        # ResponsiveGameplay.hs's build_tool_remote_warning cases).
        #
        # The warning is never bypassed, suppressed, or short-circuited
        # here: the accepted placement is reached only through the real
        # establishHere() handler, never by calling
        # commitStartingPlacement directly and never by making the arena
        # non-remote.
        send(port, "engine.loadScript('scripts/build_tool.lua', 0.0); return 'ok'")
        send(port, "world.initArena('portal_probe'); return 'ok'")
        send(port, "world.show('portal_probe'); return 'ok'")
        arena_active = False
        for _ in range(50):
            if send(port, "return world.getActiveWorldId()").strip('"') == "portal_probe":
                arena_active = True
                break
            time.sleep(0.2)
        if not arena_active:
            passed = False
            fail_setup("portal_probe arena never became active")
        else:
            send(port, "return world.loadChunksInRegion(-1, -1, 1, 1)")
            send(port, "return world.waitForChunks(30)", timeout=35)
            send(port, "camera.setPosition(0, 0); return 'ok'")
            fb = jget(port, "return {engine.getFramebufferSize()}")
            fb_w, fb_h = (fb if isinstance(fb, list) and len(fb) == 2
                          and all(isinstance(v, (int, float)) and v > 0
                                  for v in fb)
                          else (1920, 1080))
            cx, cy = fb_w / 2, fb_h / 2
            picked = jget(port, f"return {{world.pickTile({cx}, {cy})}}")
            # HUD/toolbar never initialise headless (no GPU) — stub the
            # one field handleMouseDown/enterPlacement actually read
            # (same technique as wire_probe.py's path-builder phase).
            send(port, "local bt = require('scripts.build_tool'); "
                       "bt.hud = { worldId = 'portal_probe' }; return 'ok'")
            warn_ready = jget(port,
                              "local ok, err = pcall(function() "
                              "require('scripts.build_tool_remote_warning')"
                              f".init(1, 2, 3, {fb_w}, {fb_h}) end); "
                              "return {ok = ok, err = tostring(err)}")
            tile_ok = (isinstance(picked, list) and len(picked) >= 2
                       and all(isinstance(v, (int, float))
                               and not isinstance(v, bool)
                               for v in picked[:2]))
            modal_ok = (isinstance(warn_ready, dict)
                        and warn_ready.get("ok") is True)
            # Precondition classification (#1399 requirement 6) happens
            # BEFORE any placement assertion: an unusable arena, an
            # unpickable tile or an unpreparable modal means the
            # contract went unverified, not that it was violated.
            if not tile_ok:
                passed = False
                fail_setup(f"world.pickTile({cx}, {cy}) returned no usable "
                           f"tile coordinates: {picked}")
            elif not modal_ok:
                passed = False
                fail_setup("build_tool_remote_warning could not be "
                           f"initialised headless: {warn_ready}")
            else:
                send(port, "local bt = require('scripts.build_tool'); "
                           "bt.enterPlacement{kind='building', def='acolyte_portal', "
                           "isStarting=true, displayName='Portal'}; return 'ok'")
                before_ids = active_building_ids(port)

                # --- stage 1: the click PRESENTS the warning and spawns
                # nothing. Drained independently of stage 2, so "this
                # stage recorded presented and no commitPlacement" is a
                # claim about the click's own records alone.
                send(port, "return debug.drainActionOutcomes()")  # clear noise
                click = jget(port,
                             "local bt = require('scripts.build_tool'); "
                             f"local ok, res = pcall(bt.handleMouseDown, 1, {cx}, {cy}); "
                             "return {ok = ok, consumed = (res == true), "
                             "err = (ok and '' or tostring(res))}")
                drained_click = jget(port, "return debug.drainActionOutcomes()")
                click_records = (drained_click
                                 if isinstance(drained_click, list) else [])
                armed = jget(port,
                             "local bt = require('scripts.build_tool'); "
                             "local w = require('scripts.build_tool_remote_warning'); "
                             "return {mode = bt.state.mode, "
                             "def = (bt.state.target and bt.state.target.def) or '', "
                             "warningOpen = w.isOpen()}")
                mid_ids = active_building_ids(port)

                if not (isinstance(click, dict) and click.get("ok") is True):
                    # A raise here is the pre-#1399 failure mode itself
                    # (open() dying on a nil box handle), so it is a
                    # fixture problem rather than a contract violation.
                    passed = False
                    fail_setup("buildTool.handleMouseDown raised on the "
                               f"warning-open path: {click}")
                else:
                    presented = [r for r in click_records
                                 if isinstance(r, dict)
                                 and r.get("kind") == "buildTool.remoteWarning"
                                 and r.get("outcome") == "presented"]
                    committed_early = [r for r in click_records
                                       if isinstance(r, dict)
                                       and r.get("kind") == "buildTool.commitPlacement"]
                    ok8a = bool(click.get("consumed") is True
                                and len(presented) == 1
                                and not committed_early
                                and mid_ids == before_ids
                                and isinstance(armed, dict)
                                and armed.get("mode") == "placement"
                                and armed.get("def") == "acolyte_portal"
                                and armed.get("warningOpen") is True)
                    passed &= ok8a
                    print(f"  [{'PASS' if ok8a else 'FAIL'}] the portal click "
                          f"is consumed, records remoteWarning/presented with "
                          f"no commitPlacement, spawns nothing and leaves "
                          f"placement armed: consumed="
                          f"{click.get('consumed')}, {drained_click}, "
                          f"{armed}, buildings {len(before_ids)}->"
                          f"{len(mid_ids)}")

                    # --- stage 2: the REAL confirmation handler. Its own
                    # drain again, so the required ordering (confirmed
                    # immediately followed by commitPlacement/accepted)
                    # is read off this stage's records rather than a
                    # buffer still holding stage 1's.
                    send(port, "return debug.drainActionOutcomes()")  # isolate
                    confirm = jget(port,
                                   "local w = require('scripts.build_tool_remote_warning'); "
                                   "local ok, err = pcall(w.establishHere); "
                                   "return {ok = ok, err = (ok and '' or tostring(err))}")
                    drained_confirm = jget(port, "return debug.drainActionOutcomes()")
                    confirm_records = (drained_confirm
                                       if isinstance(drained_confirm, list)
                                       else [])
                    first = (confirm_records[0]
                             if len(confirm_records) > 0
                             and isinstance(confirm_records[0], dict) else {})
                    second = (confirm_records[1]
                              if len(confirm_records) > 1
                              and isinstance(confirm_records[1], dict) else {})
                    new_ids, after_ids = wait_for_one_new_building(
                        port, before_ids)
                    ok8b = bool(isinstance(confirm, dict)
                                and confirm.get("ok") is True
                                and first.get("kind") == "buildTool.remoteWarning"
                                and first.get("outcome") == "confirmed"
                                and second.get("kind") == "buildTool.commitPlacement"
                                and second.get("outcome") == "accepted"
                                and second.get("reason") is None
                                and len(new_ids) == 1
                                and after_ids == before_ids | new_ids)
                    passed &= ok8b
                    print(f"  [{'PASS' if ok8b else 'FAIL'}] establishHere() "
                          f"records remoteWarning/confirmed then "
                          f"commitPlacement/accepted with no reason, and "
                          f"exactly one new building appears: "
                          f"{drained_confirm}, new ids {sorted(new_ids)} "
                          f"(buildings {len(before_ids)}->{len(after_ids)})")

        # --- 9: the Lua-recorded game-world F4 producer shares F1/F2/F3's
        # framebuffer-pixel oracle space (#774) ---
        #
        # scripts/unit_drag_select.lua's recordDeferredClick/
        # recordDragOutcome are the real producers behind every
        # game-world click/drag Layer-A record (scripts/init_mouse.lua's
        # own recordClick just forwards into dragSelect.deferClick). The
        # engine-recorded half of this contract already has a synthetic
        # 2x-scale regression in the Input.LayerA hspec suite; that
        # harness can't cover this half (it boots neither the input nor
        # the Lua thread, so these Lua producers never run there). A real
        # headless engine always boots window==framebuffer 1:1 with no
        # live resize path to diverge them, so this monkey-patches
        # engine.getWindowSize/getFramebufferSize for one check — saving
        # the originals first and restoring them immediately after, so
        # nothing later in this probe (or a real player session) is
        # affected — to prove the conversion this module performs, not
        # just its arithmetic in isolation.
        send(port, "engine.loadScript('scripts/unit_drag_select.lua', 0.0); "
                   "return 'ok'")
        send(port, "return debug.drainActionOutcomes()")  # clear noise
        send(port,
             "_G._probe774OrigWS = engine.getWindowSize; "
             "_G._probe774OrigFS = engine.getFramebufferSize; "
             "engine.getWindowSize = function() return 1280, 720 end; "
             "engine.getFramebufferSize = function() return 2560, 1440 end; "
             "return 'ok'")
        send(port,
             "local ds = require('scripts.unit_drag_select'); "
             "ds.handleMouseDown(1, 100, 50); "
             "ds.deferClick(1, 'probe774_handler', 'accepted', 100, 50, nil); "
             "ds.onMouseUp(1, 100, 50, 'game'); "
             "return 'ok'")
        drained_lua_scale = jget(port, "return debug.drainActionOutcomes()")
        send(port,
             "engine.getWindowSize = _G._probe774OrigWS; "
             "engine.getFramebufferSize = _G._probe774OrigFS; "
             "_G._probe774OrigWS = nil; _G._probe774OrigFS = nil; "
             "return 'ok'")
        lua_rec = (drained_lua_scale[0]
                   if isinstance(drained_lua_scale, list) and drained_lua_scale
                   else {})
        ok9 = bool(lua_rec.get("kind") == "input.click"
                   and lua_rec.get("handler") == "probe774_handler"
                   and lua_rec.get("where") == {"x": 200, "y": 100})
        passed &= ok9
        print(f"  [{'PASS' if ok9 else 'FAIL'}] the Lua-recorded game-world "
              f"click producer (unit_drag_select's recordDeferredClick) "
              f"converts a window-space (100,50) click to framebuffer "
              f"(200,100) at a 2x window-to-framebuffer scale: "
              f"{drained_lua_scale}")

        print("\n" + ("ALL ACTION-OUTCOME CHECKS PASSED" if passed else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
