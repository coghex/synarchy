#!/usr/bin/env python3
"""Headless follow_command-priority probe (#306).

Guards the utility ladder around FOLLOW_COMMAND_UTILITY (7.0): actions
documented to outrank a player move order must actually win the AI
arbitration. The bug (#306) was that follow_command was retuned 1.0 -> 7.0
without re-deriving the actions calibrated to beat it, so a pending move
order silently suppressed an explicit pickup and a dry-canteen refill.

Unlike movement_probe.py this keeps the unit_ai tick LIVE (the arbitration
IS the thing under test) and reads the chosen action straight off the AI
state via unitAi.getState(uid).currentAction.

The target ladder (#306): dire-survival > combat/treatment > player orders
(move/pickup/dry-refill) > situational goals (find_water/notify) > work/wander.

Checks (each on a fresh acolyte on a flat arena):
  A. pickup_ground (a peer player order) beats a pending move.
  B. refill_canteen (dry) beats a pending move.
  C. a move order beats a ROUTINE goal (a fresh acolyte's find_water search).
  D. combat beats a goal: a struck goal-bound acolyte reacts with a combat
     action instead of continuing to search.
  E. combat COMMITS over a pending move: a unit under a move order that is
     attacked enters combat and stays there (attack_target out-ranks the
     pending follow_command, so engage's hand-off isn't yanked back).
  F. the OTHER side of A's precondition: an over-capacity commandPickup is
     refused, stores no pickupOrder, and leaves the pending move selected.

A and F are two halves of one contract (#1736). commandPickup gates capacity
at COMMAND time (#920) and, on refusal, deliberately stores no pickupOrder;
pickupUtility then scores -inf, so follow_command (7.0) is the CORRECT winner.
Check A used to send commandPickup, discard its Boolean result and poll — so
on a weak carrying_capacity roll it read that correct outcome as a ladder
regression. Both checks now STAGE the capacity answer explicitly (an additive
carrying_capacity modifier sized off the carrier's own live load) and assert
the admission decision before judging arbitration, so neither outcome depends
on body generation and a phase A failure means a real arbitration regression.

Run from the repo/worktree root (scripts/ resolve relative to CWD):
  python3 tools/follow_command_priority_probe.py [--port N] [--unit acolyte]

Exit code 0 = all checks passed; 1 = a graded check failed; 2 = a fixture
never established itself, so nothing downstream of it was graded.
"""
from __future__ import annotations

import argparse
import glob
import json
import socket
import subprocess
import sys
import time
from probelib import quit_engine, boot, send, send_json

LOG = "/tmp/follow_command_priority_probe.log"


def bootstrap(port: int) -> None:
    """Load the YAML defs + the stat/resource ticks the loading screen would
    normally load. unit_ai is auto-loaded at boot and we deliberately leave
    its tick RUNNING — the arbitration is what we're testing."""
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
    send(port, "engine.loadScript('scripts/unit_resources.lua', 0.2); return 'ok'")
    send(port, "require('scripts.movement_arena'); return 'ok'")


def wait_world_ready(port: int) -> bool:
    for _ in range(40):
        r = send_json(port, "return world.getChunkInfo(0,0)")
        if isinstance(r, dict) and r.get("loaded"):
            return True
        time.sleep(0.25)
    return False


def current_action(port: int, uid: int) -> str:
    # The debug console serialises a returned Lua string JSON-style (with
    # surrounding quotes); strip them so callers compare bare action names.
    raw = send(port,
               f"local s=require('scripts.unit_ai').getState({uid}); "
               f"return s and s.currentAction or 'nil'")
    try:
        v = json.loads(raw)
        return v if isinstance(v, str) else raw
    except (json.JSONDecodeError, ValueError):
        return raw


def poll_for_action(port: int, uid: int, want, seconds: float = 8.0):
    """Poll currentAction; return (timeline, hit_bool). `want` is a single
    action name or a set/collection of acceptable names. Stops early on hit."""
    wants = {want} if isinstance(want, str) else set(want)
    seen: list[str] = []
    steps = int(seconds / 0.4)
    for _ in range(steps):
        a = current_action(port, uid)
        if not seen or seen[-1] != a:
            seen.append(a)
        if a in wants:
            return seen, True
        time.sleep(0.4)
    return seen, False


COMBAT_ACTIONS = {"engage", "attack_target", "retreat"}


def spawn_acolyte(port: int, unit: str, x: float, y: float) -> int:
    uid = int(float(send(port, f"return unit.spawn('{unit}', {x}, {y})")))
    time.sleep(1.5)  # settle onto ground
    return uid


def spawn_ground(port: int, def_name: str, x: int, y: int):
    """Spawn one ground item; return its gid, or None after reporting."""
    raw = send(port, f"return item.spawnGround('{def_name}', {x}, {y})")
    try:
        return int(float(raw))
    except (ValueError, TypeError):
        print(f"FAIL: could not spawn ground item '{def_name}' (got {raw!r})",
              file=sys.stderr)
        return None


def stage_pickup(port: int, uid: int, gid: int, admit: bool,
                 margin: float = 1.0):
    """Stage the command-time capacity answer, command the pickup, and read
    back what it decided — all in ONE console transaction.

    Why staged at all (#1736): carrying_capacity is a per-body random roll
    (~11-49 kg, src/Unit/Thread/Command/Body.hs) and an acolyte spawns
    carrying most of its ~12 kg kit, so whether commandPickup admits a
    10 kg chunk depends on body generation. Neither of this probe's two
    pickup checks is about that. So each one pins the carrier's EFFECTIVE
    capacity to its own live carried weight plus a stated spare:
    ``g.weight + margin`` to admit, ``margin`` alone (< the item) to
    refuse. The command-time gate still RUNS on the real numbers — it is
    given a deterministic answer, not bypassed.

    It is an additive stat MODIFIER rather than unit.setStat because the
    physiology tick calls unit.recomputeBody on every body-mass change
    (scripts/unit_resource_energy.lua), which rewrites the BASE stat and
    would silently undo a set. Modifiers live beside the base
    (uiModifiers) and survive it.

    One transaction because the unit_ai tick is LIVE here: reading the
    admission and the resulting pickupOrder in separate console round
    trips lets the AI act between them.
    """
    spare = f"(g.weight + {margin})" if admit else f"{margin}"
    lua = (
        f"local ai=require('scripts.unit_ai'); "
        f"local g,res=item.getGroundForUnit({uid},{gid}); "
        f"local carried=unit.getCarryingWeight({uid}) or 0; "
        f"local cap0=unit.getStat({uid},'carrying_capacity') or 0; "
        f"if g then unit.addModifier({uid},'carrying_capacity',"
        f"carried+{spare}-cap0,'probe_staged_capacity') end; "
        f"local ok=(g~=nil) and ai.commandPickup({uid},{gid}) or false; "
        f"local s=ai.getState({uid}); "
        f"return {{present=(g~=nil), resolved=(res and true or false), "
        f"weight=(g and g.weight or 0), carried=carried, "
        f"capacity=(unit.getStat({uid},'carrying_capacity') or 0), "
        f"accepted=(ok and true or false), "
        f"order=((s and s.pickupOrder and s.pickupOrder.gid) or -1)}}")
    return send_json(port, lua)


def precondition_failure(staged, uid: int, gid: int, want_accepted: bool):
    """Judge a `stage_pickup` result as SETUP, not behavior.

    Returns a message when the staging did not establish what the check
    is about to judge, or None when it did. An unestablished fixture is
    an exit-2 stop, never a graded check (#1396): a check that never had
    a live pickup order to score cannot say anything about the ladder.
    """
    if not isinstance(staged, dict):
        return f"the staging transaction returned {staged!r}"
    for key in ("present", "resolved", "weight", "carried", "capacity",
                "accepted", "order"):
        if key not in staged:
            return f"the staging transaction omitted {key!r} ({staged!r})"
    if not staged["present"]:
        return (f"ground item {gid} is not on unit {uid}'s own page "
                f"(page resolved: {staged['resolved']}) — the target must be "
                f"present for the capacity gate to be what decides")
    total = staged["carried"] + staged["weight"]
    if want_accepted:
        if not staged["accepted"]:
            return (f"commandPickup REFUSED the order at the command-time "
                    f"capacity gate (#920): {staged['carried']:.2f} kg carried "
                    f"+ {staged['weight']:.2f} kg item = {total:.2f} kg over a "
                    f"{staged['capacity']:.2f} kg capacity, so no pickupOrder "
                    f"was stored and follow_command is the correct winner")
        if staged["order"] != gid:
            return (f"commandPickup returned true but the unit's pickupOrder "
                    f"is {staged['order']}, not the staged gid {gid} — there "
                    f"is no live order to score")
    elif total <= staged["capacity"]:
        return (f"the staging left the target liftable: {staged['carried']:.2f} "
                f"kg carried + {staged['weight']:.2f} kg item = {total:.2f} kg "
                f"within a {staged['capacity']:.2f} kg capacity, so a refusal "
                f"would not be the capacity gate's doing")
    return None


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--unit", default="acolyte")
    ap.add_argument("--port", type=int, default=9163)
    ap.add_argument("--seconds", type=float, default=8.0)
    args = ap.parse_args()

    proc = boot(args.port, log=LOG)
    try:
        bootstrap(args.port)
        course = send_json(
            args.port,
            "return require('scripts.movement_arena').buildCourse('flat')")
        if not isinstance(course, dict) or "sx" not in course:
            print("FAIL: could not build flat arena", file=sys.stderr)
            return 2
        if not wait_world_ready(args.port):
            print("FAIL: arena world never became queryable", file=sys.stderr)
            return 2
        sx, sy = course["sx"] + 0.5, course["sy"] + 0.5
        far_x, far_y = course["gx"] + 0.5, course["gy"] + 0.5  # far move target
        print(f"flat arena: spawn ({sx},{sy}), far move target ({far_x},{far_y})")

        checks: list[tuple[str, bool]] = []

        # --- Check A: explicit pickup beats a pending move order ----------
        ua = spawn_acolyte(args.port, args.unit, sx, sy)
        if ua < 0:
            print(f"FAIL: could not spawn '{args.unit}'", file=sys.stderr)
            return 2
        # A 10 kg granite chunk two tiles away — NOT a light item, and
        # that is the point: the staging below is what makes it liftable,
        # not the def (#1736).
        ix, iy = int(sx) + 2, int(sy)
        gid = spawn_ground(args.port, "granite_chunk", ix, iy)
        if gid is None:
            return 2
        # Move order to the far edge first, THEN an explicit pickup. Both
        # commandedTask and pickupOrder are now live; pickup (7.5) must
        # win over follow_command (7.0).
        send(args.port,
             f"require('scripts.unit_ai').commandMove({ua},{far_x},{far_y}); "
             f"return 'moved'")
        staged = stage_pickup(args.port, ua, gid, admit=True)
        note = precondition_failure(staged, ua, gid, want_accepted=True)
        if note:
            print(f"FAIL: check A precondition — {note}", file=sys.stderr)
            return 2
        print(f"[A staged] carried {staged['carried']:.2f} kg + item "
              f"{staged['weight']:.2f} kg vs a staged {staged['capacity']:.2f} kg "
              f"capacity; commandPickup accepted, pickupOrder live on gid "
              f"{staged['order']}")
        seen, hit = poll_for_action(args.port, ua, "pickup_ground", args.seconds)
        print(f"\n[A pickup-beats-move] action timeline: {' -> '.join(seen)}")
        checks.append(("pickup_ground wins over a pending move order", hit))

        # --- Check B: a dry-canteen refill beats a pending move order -----
        ub = spawn_acolyte(args.port, args.unit, sx + 1, sy + 1)
        # Empty the canteen (acolytes spawn with a full 2 L canteen).
        send(args.port,
             f"unit.modifyItemFill({ub}, 'canteen_steel_2l', -5.0); return 'drained'")
        # Inject a known water source adjacent so refill has somewhere to go
        # (the FOV scanner would find it too, but injection is deterministic).
        wx, wy = int(sx) + 3, int(sy) + 3
        send(args.port,
             f"local s=require('scripts.unit_ai').getState({ub}); "
             f"s.knownWaterSources = {{{{x={wx},y={wy}}}}}; return 'water'")
        send(args.port,
             f"require('scripts.unit_ai').commandMove({ub},{far_x},{far_y}); "
             f"return 'moved'")
        seenb, hitb = poll_for_action(args.port, ub, "refill_canteen", args.seconds)
        print(f"\n[B refill-beats-move] action timeline: {' -> '.join(seenb)}")
        checks.append(("refill_canteen (dry) wins over a pending move order", hitb))

        # --- Check C: a move order beats a ROUTINE goal. A fresh acolyte
        # spawns with an active find_water goal and no known water, so absent
        # other input it searches (search_for_water). Issuing a move must pull
        # it onto follow_command -> a routine goal yields to a player order
        # (command > goals; the inversion this fix corrects).
        uc = spawn_acolyte(args.port, args.unit, sx + 2, sy + 2)
        # Confirm it is searching first (goal is live, no water known).
        pre = current_action(args.port, uc)
        send(args.port,
             f"require('scripts.unit_ai').commandMove({uc},{far_x},{far_y}); "
             f"return 'moved'")
        seenc, hitc = poll_for_action(args.port, uc, "follow_command", args.seconds)
        print(f"\n[C move-beats-goal] pre={pre}  timeline: {' -> '.join(seenc)}")
        checks.append(("follow_command beats a routine find_water goal", hitc))

        # --- Check D: combat beats a goal. Stage an attacker next to a
        # goal-bound acolyte (searching for water). When struck it must react
        # with a combat action (engage/attack_target/retreat), NOT keep
        # searching -- self-defense outranks a routine goal.
        ud = spawn_acolyte(args.port, args.unit, sx + 4, sy)
        atk = spawn_acolyte(args.port, args.unit, sx + 5, sy)  # adjacent
        send(args.port,
             f"require('scripts.unit_ai').commandAttack({atk},{ud}); return 'fight'")
        seend, hitd = poll_for_action(args.port, ud, COMBAT_ACTIONS, args.seconds + 6)
        print(f"\n[D combat-beats-goal] victim timeline: {' -> '.join(seend)}")
        checks.append(("a struck goal-bound unit reacts with combat, not search",
                       hitd))

        # --- Check E: combat COMMITS over a pending move (issue: engage hands
        # off to attack_target, which must stay above follow_command or the
        # stale move yanks the unit off the fight). Give the victim a move
        # order FIRST, then attack it; it must reach a combat action AND not
        # fall back to follow_command while the fight is on.
        ue = spawn_acolyte(args.port, args.unit, sx + 7, sy)
        send(args.port,
             f"require('scripts.unit_ai').commandMove({ue},{far_x},{far_y}); return 'moved'")
        atk2 = spawn_acolyte(args.port, args.unit, sx + 8, sy)  # adjacent
        send(args.port,
             f"require('scripts.unit_ai').commandAttack({atk2},{ue}); return 'fight'")
        seene, hite = poll_for_action(args.port, ue, COMBAT_ACTIONS, args.seconds + 6)
        # Once fighting, sample a few more times: it must NOT revert to the move.
        tail = []
        for _ in range(8):
            time.sleep(0.4)
            tail.append(current_action(args.port, ue))
        reverted = "follow_command" in tail
        print(f"\n[E combat-commits-over-move] timeline: {' -> '.join(seene)}"
              f"  | tail: {' '.join(tail)}")
        checks.append(("combat reached over a pending move", hite))
        checks.append(("combat does NOT hand back to the stale move", not reverted))

        # --- Check F: the command-time capacity refusal, asserted rather
        # than observed. A's mirror image (#1736): commandPickup gates
        # capacity at COMMAND time (#920), and a refusal deliberately
        # stores no pickupOrder so a position hold survives it (#1216).
        # pickupUtility then scores -inf, leaving the pending move order
        # the correct winner -- which is exactly the outcome check A used
        # to misread as a ladder regression. Staged over capacity here, so
        # it is the capacity path and not commandPickup's separate
        # missing/off-page-item refusal that is under test.
        uf = spawn_acolyte(args.port, args.unit, sx, sy + 6)
        gidf = spawn_ground(args.port, "granite_chunk",
                            int(sx) + 2, int(sy) + 6)
        if gidf is None:
            return 2
        send(args.port,
             f"require('scripts.unit_ai').commandMove({uf},{far_x},{far_y}); "
             f"return 'moved'")
        refused = stage_pickup(args.port, uf, gidf, admit=False)
        notef = precondition_failure(refused, uf, gidf, want_accepted=False)
        if notef:
            print(f"FAIL: check F precondition — {notef}", file=sys.stderr)
            return 2
        print(f"\n[F staged] carried {refused['carried']:.2f} kg + item "
              f"{refused['weight']:.2f} kg vs a staged "
              f"{refused['capacity']:.2f} kg capacity")
        checks.append(("an over-capacity commandPickup returns false",
                       refused["accepted"] is False))
        checks.append(("a refused pickup stores no pickupOrder",
                       refused["order"] == -1))
        seenf, hitf = poll_for_action(args.port, uf, "follow_command",
                                      args.seconds)
        print(f"[F refusal-keeps-move] action timeline: {' -> '.join(seenf)}")
        checks.append(("a refused pickup leaves the pending move selected",
                       hitf))

        print("\n--- checks ---")
        all_ok = True
        for label, ok in checks:
            print(f"  [{'PASS' if ok else 'FAIL'}] {label}")
            all_ok = all_ok and ok
        return 0 if all_ok else 1
    finally:
        quit_engine(args.port, proc)


if __name__ == "__main__":
    sys.exit(main())
