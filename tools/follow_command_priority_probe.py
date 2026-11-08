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
  E. combat COMMITS over a pending NON-PLAYER move: a unit under an
     internal/ordinary move order (``commandMove(..., internal=true)``, 7.0)
     that is attacked enters combat and stays there (attack_target, 8.0,
     out-ranks the pending follow_command, so engage's hand-off isn't
     yanked back). Narrowed in #2945: #916 superseded the #306 rule that
     every attack outranks every pending move. A PLAYER move now scores 9.0
     against an active attack goal (scripts/unit_ai_combat_move.lua) and may
     pull the unit out of combat, so E asserts nothing about it; player
     withdrawal stays covered by the Hspec group ``player move orders
     during combat (#916)`` (test-headless/Test/Headless/Lua/
     UnitAiCombatMove.hs).
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

D and E stage their fight with a COMMITTED attack order (``commandAttack(
attacker, victim, true)``), with the attacker placed beside the victim at
order time (``stage_committed_attack``). An uncommitted order opens an "autonomous",
non-exempt attack occurrence, and #2521's combat policy ends that goal
before any swing for two same-faction acolytes (neither is hostile to the
other, the attacker has no last attacker, and the production fixture inputs
authorize nothing), so on a plain ``commandAttack`` the staged attacker
never landed a hit (#2945 before-batch: 0 of 20 trials). The victim is still
graded on its OWN arbitration: it reacts through self-defense once hits land.
Each combat pair is destroyed when its check is done (``destroy_units``).
This is evidence-backed, not speculative (#2945): with the pairs left on the
arena, 6 of 20 trials at e9da94adc failed with ``treat_ally`` selected, and the
same-transaction snapshots show why - D's wounded survivors sat 3-4 tiles from
E's subject (which was already ``treat_ally`` BEFORE its attack was ordered in
3 trials), and D's/E's survivors were within a few tiles of F's subject in 5.
A medic treating an adjacent wounded ally at 8.0 over a 7.0 move order is the
documented ladder, so those were probe fixtures leaving patients the checks do
not intend, not arbitration faults. The ``wounded`` list stays in the output
so a patient that survives anyway is visible.

Each check prints an ``observe`` snapshot (goal, pending move, treatment
state, last attacker, wounded units, combat eligibility) read in the same
console transaction as the action it sits beside; see ``observe``.

Run from the repo/worktree root (scripts/ resolve relative to CWD):
  python3 tools/follow_command_priority_probe.py [--port N] [--unit acolyte]

Exit code 0 = all checks passed; 1 = a graded check failed; 2 = a fixture
never established itself, so nothing downstream of it was graded; 3 = check
E's SETUP failed (the staged attacker never landed a qualifying hit on E's
subject, or E's non-player move was no longer pending in the observation carrying that hit,
so E's arbitration was never exercised) and no other graded check
failed. 3 is a failure, never a pass, and is distinct from a graded
arbitration failure (exit 1: a hit landed and the subject still did not
reach or hold combat).
"""
from __future__ import annotations

import argparse
import json
import socket
import subprocess
import sys
import time
from probelib import (quit_engine, boot, send, send_json, load_definitions)

LOG = "/tmp/follow_command_priority_probe.log"


def bootstrap(port: int) -> None:
    """Load the YAML defs + the stat/resource ticks the loading screen would
    normally load. unit_ai is auto-loaded at boot and we deliberately leave
    its tick RUNNING — the arbitration is what we're testing."""
    load_definitions(port, (
        "substance", "item", "equipment", "material", "faction", "physiology",
        "unit"))
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

# --------------------------------------------------------------------------
# Evidence capture (#2945). Printing only: nothing here is graded.
# --------------------------------------------------------------------------
# uid -> the phase that spawned it ("E", "E-attacker", ...), so a wounded unit
# on the arena can be attributed to the check that left it there.
PHASE_OF: dict[int, str] = {}

# Mirrors ENGAGE_WINDOW_SEC (scripts/unit_ai_combat.lua): the incoming_hit
# source only counts a hit this recent.
HIT_WINDOW_SEC = 10.0


def observe(port: int, uid: int, attacker=None,
            keep_adjacent: bool = False) -> dict:
    """One console transaction snapshotting everything the arbitration reads.

    Action, goal, pending move, treatment state, the last-attacker record,
    the staged attacker's identity/pose and the wounded units on the arena
    are read in ONE round trip so they describe the same instant (the AI
    tick is live: separate round trips let it act in between). The
    ``incoming_hit_scores`` / ``attack_goal_scores`` fields restate the two
    combat sources' eligibility (unit_ai_combat.lua's incoming_hit gate and
    unit_ai_combat_attack.lua's attack goal, both 8.0) so a treat_ally
    action can be read against whether combat was eligible to beat it.

    ``keep_adjacent`` (D and E polls only): until the staged attacker has
    landed its first hit on ``uid``, re-place it beside ``uid`` at the start
    of the same transaction. The victim keeps walking and an acolyte chasing
    an equal-speed walker may never close (2 of 20 trials at 46401665f, and
    2 of 20 at 0bdd3b56f). Staging only: once a hit lands nothing is moved
    and the victim's reaction is its own arbitration.
    """
    labels = ",".join(f"[{u}]='{l}'" for u, l in sorted(PHASE_OF.items()))
    atk = "nil" if attacker is None else str(int(attacker))
    adj = (f"if {1 if keep_adjacent else 0}==1 and {atk}~=nil "
           f"and unit.exists({atk}) then "
           f"local a0=unit.getLastAttacker({int(uid)}); "
           f"if not (a0 and a0.uid=={atk}) then "
           f"local v=unit.getInfo({int(uid)}); "
           f"if v then unit.setPos({atk}, v.gridX+0.6, v.gridY) end end end;")
    lua = " ".join([
        adj,
        "local ai=require('scripts.unit_ai');",
        "local pol=require('scripts.unit_ai_combat_policy');",
        f"local uid={int(uid)}; local atk={atk}; local L={{{labels}}};",
        "local now=engine.gameTime(); local s=ai.getState(uid) or {};",
        "local me=unit.getInfo(uid); local att=unit.getLastAttacker(uid);",
        "local function pose(u) return (u and unit.exists(u)) and unit.getPose(u) or nil end;",
        "local function pos(u) local i=u and unit.getInfo(u);",
        "return i and {x=i.gridX,y=i.gridY,page=i.page} or nil end;",
        "local ct=s.commandedTask;",
        "local o={action=s.currentAction or 'nil', goal=s.activeGoal,",
        "attackTarget=s.attackTargetUid, now=now,",
        "movePending=(ct~=nil), movePlayer=((ct and ct.player) and true or false),",
        "treatClaim=(s.treatClaim and s.treatClaim.patient) or nil,",
        "treatPending=(s.treatPending and s.treatPending.uid) or nil,",
        "pose=pose(uid), pos=pos(uid), attackerUid=atk, attackerPose=pose(atk),",
        "attackerPos=pos(atk)};",
        "local as=atk and ai.getState(atk); if as then o.attackerAction=as.currentAction;",
        "o.attackerGoal=as.activeGoal; o.attackerTarget=as.attackTargetUid end;",
        "if att then o.lastAttacker={uid=att.uid, at=att.at, age=now-(att.at or 0)} end;",
        f"local win={HIT_WINDOW_SEC};",
        "o.hitByStaged=((atk~=nil) and att~=nil and att.uid==atk and (now-(att.at or 0))<=win) and true or false;",
        "local elig=false; local basis=nil;",
        "if att and unit.exists(att.uid) and unit.getPose(att.uid)~='dead' and (now-(att.at or 0))<=win then",
        "local hold=s.holdAnchor; local held=hold and hold.combatWithdrawalCompletedAt and (att.at or 0)<=hold.combatWithdrawalCompletedAt;",
        "if not held then local ok,b=pcall(pol.acquireBasis,uid,att.uid,att); if ok and b then elig=true; basis=b end end end;",
        "o.incomingHitScores=elig; o.incomingHitBasis=basis;",
        "o.attackGoalScores=((s.activeGoal=='attack') and s.attackTargetUid~=nil) and true or false;",
        "local w={}; for _,u in ipairs(unit.getAllIds() or {}) do local ws=unit.getWounds(u) or {};",
        "if #ws>0 then local i=unit.getInfo(u); local k={};",
        "for _,x in ipairs(ws) do k[#k+1]={kind=x.kind, bandage=x.bandage, clot=x.clot, infection=x.infection} end;",
        "w[#w+1]={uid=u, phase=L[u] or '?', wounds=#ws, records=k,",
        "page=i and i.page, x=i and i.gridX, y=i and i.gridY,",
        "dist=(i and me) and math.sqrt((i.gridX-me.gridX)^2+(i.gridY-me.gridY)^2) or nil} end end;",
        "o.wounded=w; return o",
    ])
    r = send_json(port, lua)
    if not isinstance(r, dict):
        return {"action": str(r), "observeError": True}
    if not r.get("wounded"):
        r["wounded"] = []  # the console renders an empty Lua table as {}
    return r


def poll_observed(port: int, uid: int, want, seconds: float, attacker=None,
                  sink: list | None = None):
    """``poll_for_action`` with an ``observe`` snapshot taken in the same
    transaction as every action sample. Returns
    (timeline, hit, transition_snapshots, last_snapshot). Same stop rule and
    cadence as ``poll_for_action``; the snapshots are only printed. Every
    sample is also appended to ``sink`` when given."""
    wants = {want} if isinstance(want, str) else set(want)
    seen: list[str] = []
    snaps: list[dict] = []
    last: dict = {}
    for _ in range(int(seconds / 0.4)):
        last = observe(port, uid, attacker, keep_adjacent=attacker is not None)
        if sink is not None:
            sink.append(last)
        a = last.get("action", "nil")
        if not seen or seen[-1] != a:
            seen.append(a)
            snaps.append(last)
        if a in wants:
            return seen, True, snaps, last
        time.sleep(0.4)
    return seen, False, snaps, last


def stage_committed_attack(port: int, attacker: int, victim: int,
                           beside: float = 0.6) -> None:
    """Put ``attacker`` right beside ``victim``'s CURRENT position and issue
    the COMMITTED attack order, in one console transaction (#2945).

    The victim is a live, goal-bound acolyte that walks (search_for_water /
    the pending move). Spawning the attacker "adjacent" and then settling
    both for 1.5 s each left it a few tiles behind an equal-speed target,
    so the chase sometimes never closed inside the poll window and no hit
    landed (2 of 20 after-batch trials at 0bdd3b56f, both check D). Placing
    the attacker at order time makes the first swing immediate. Only the
    staging changes: the victim's reaction is still its own arbitration."""
    send(port,
         f"local v=unit.getInfo({int(victim)}); "
         f"unit.setPos({int(attacker)}, v.gridX+{beside}, v.gridY); "
         f"require('scripts.unit_ai').commandAttack({int(attacker)},"
         f"{int(victim)},true); return 'fight'")


def destroy_units(port: int, *uids: int) -> None:
    """Remove a finished phase's units so nothing they wounded is left on
    the arena for a later phase's acolytes to treat (see the docstring)."""
    for u in uids:
        send(port, f"unit.destroy({int(u)}); return 'ok'")


def grade_e(samples: list, tail_obs: list):
    """Classify check E from its observations (#2945). Pure: no engine.

    Returns ``(setup_reason, reached, reverted)``.

    * ``setup_reason`` is None when E's arbitration was really exercised,
      else why it was not: the staged attacker never landed a qualifying
      hit, or the subject's NON-PLAYER move was not still pending in the
      qualifying combat observation. That observation is the first sample
      whose action is a combat action (or, if combat was never reached, the
      first sample carrying a hit). A move that arrived, or was cleaned up
      by ``maintainTask``, before combat is observed leaves nothing for
      combat to out-rank, so both E assertions would pass vacuously.
    * ``reached``: some sample's action was a combat action.
    * ``reverted``: a tail sample handed back to ``follow_command`` while
      the staged attacker was still alive ("while the fight is on").
    """
    landed = any(o.get("hitByStaged") for o in samples)
    ref = next((o for o in samples if o.get("action") in COMBAT_ACTIONS), None)
    if ref is None:
        ref = next((o for o in samples if o.get("hitByStaged")), None)
    move_ok = (ref is not None and bool(ref.get("movePending"))
               and not ref.get("movePlayer"))
    reached = any(o.get("action") in COMBAT_ACTIONS for o in samples)
    reverted = any(o.get("action") == "follow_command"
                   and o.get("attackerPose") != "dead" for o in tail_obs)
    if not landed:
        reason = ("the staged attacker never landed a qualifying hit on the "
                  f"subject within {HIT_WINDOW_SEC:g} s")
    elif not move_ok:
        reason = ("the subject's non-player move was no longer pending in "
                  "the qualifying combat observation")
    else:
        reason = None
    return reason, reached, reverted


def print_obs(tag: str, snaps: list, last: dict) -> None:
    """Print each action-transition snapshot plus the final sample."""
    def line(label, o):
        print(f"  [{tag} obs {label}] "
              f"{json.dumps(o, sort_keys=True, separators=(',', ':'))}")
    for i, o in enumerate(snaps):
        line(f"transition {i + 1} -> {o.get('action')}", o)
    if last and (not snaps or last is not snaps[-1]):
        line("final", last)



def spawn_acolyte(port: int, unit: str, x: float, y: float,
                  label: str = "?") -> int:
    uid = int(float(send(port, f"return unit.spawn('{unit}', {x}, {y})")))
    PHASE_OF[uid] = label
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
        e_setup_failed = False

        # --- Check A: explicit pickup beats a pending move order ----------
        ua = spawn_acolyte(args.port, args.unit, sx, sy, "A")
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
        ub = spawn_acolyte(args.port, args.unit, sx + 1, sy + 1, "B")
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
        uc = spawn_acolyte(args.port, args.unit, sx + 2, sy + 2, "C")
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
        ud = spawn_acolyte(args.port, args.unit, sx + 4, sy, "D")
        atk = spawn_acolyte(args.port, args.unit, sx + 5, sy, "D-attacker")  # adjacent
        # COMMITTED order (#2945): see the module docstring.
        stage_committed_attack(args.port, atk, ud)
        sampd: list = []
        seend, hitd, snapd, lastd = poll_observed(
            args.port, ud, COMBAT_ACTIONS, args.seconds + 6, attacker=atk,
            sink=sampd)
        print(f"\n[D combat-beats-goal] victim timeline: {' -> '.join(seend)}")
        print_obs("D", snapd, lastd)
        print(f"  [D staged attacker landed a qualifying hit: "
              f"{any(o.get('hitByStaged') for o in sampd)}]")
        destroy_units(args.port, ud, atk)
        checks.append(("a struck goal-bound unit reacts with combat, not search",
                       hitd))

        # --- Check E: combat COMMITS over a pending NON-PLAYER move (engage
        # hands off to attack_target, which must stay above follow_command
        # or the stale move yanks the unit off the fight). Give the victim an
        # INTERNAL move order FIRST (commandMove's ``internal`` argument: no
        # ``commandedTask.player``, so it keeps the ordinary 7.0), then
        # attack it; it must reach a combat action AND not fall back to
        # follow_command while the fight is on. A PLAYER move is out of
        # scope since #916 scores it 9.0 against an active attack and lets
        # the player withdraw (#2945; Hspec "player move orders during
        # combat (#916)" owns that).
        # Spawned at the course start, ~9 tiles from the far target, so the
        # internal move is still pending when the attack lands (it used to
        # spawn 2 tiles from the target and could arrive first, leaving
        # nothing for combat to out-rank; #2945).
        ue = spawn_acolyte(args.port, args.unit, sx, sy + 3, "E")
        send(args.port,
             f"require('scripts.unit_ai').commandMove({ue},{far_x},{far_y},"
             f"nil,true); return 'moved'")
        atk2 = spawn_acolyte(args.port, args.unit, sx + 1, sy + 3, "E-attacker")
        # The move must still be pending, and non-player, at the moment the
        # attack is ordered; otherwise there is nothing for combat to beat.
        pre_e = observe(args.port, ue, atk2)
        print(f"  [E pre-attack] movePending={pre_e.get('movePending')} "
              f"movePlayer={pre_e.get('movePlayer')} "
              f"action={pre_e.get('action')}")
        stage_committed_attack(args.port, atk2, ue)
        sampe: list = []
        seene, hite, snape, laste = poll_observed(
            args.port, ue, COMBAT_ACTIONS, args.seconds + 6, attacker=atk2,
            sink=sampe)
        # Once fighting, sample a few more times: it must NOT revert to the move.
        tail = []
        tail_obs = []
        for _ in range(8):
            time.sleep(0.4)
            o = observe(args.port, ue, atk2)
            sampe.append(o)
            tail_obs.append(o)
            tail.append(o.get("action", "nil"))
        # "While the fight is on": a sample taken after the staged attacker
        # died is not a hand-back (the fight ended on its own, and the
        # pending move legitimately resumes). At e9da94adc one trial's
        # victim killed its attacker mid-tail and read as a hand-back.
        over = sum(1 for o in tail_obs if o.get("attackerPose") == "dead")
        e_why, e_reached, reverted = grade_e(sampe, tail_obs)
        if over:
            print(f"  [E tail: {over} of {len(tail_obs)} samples taken after "
                  f"the staged attacker died are not counted as hand-back]")
        print(f"\n[E combat-commits-over-move] timeline: {' -> '.join(seene)}"
              f"  | tail: {' '.join(tail)}")
        print_obs("E", snape, laste)
        # Every tail action transition (a treat_ally episode included), then
        # the final sample: each carries its own same-transaction patient,
        # claim and combat-eligibility evidence.
        prev = seene[-1] if seene else None
        for i, o in enumerate(tail_obs):
            if o.get("action") != prev or i == len(tail_obs) - 1:
                print(f"  [E obs tail {i + 1} -> {o.get('action')}] "
                      f"{json.dumps(o, sort_keys=True, separators=(',', ':'))}")
            prev = o.get("action")
        # Requirement 3 (#2945): tell a SETUP failure (the staged attacker
        # never landed a qualifying hit, so E's arbitration was never
        # exercised) from an ARBITRATION failure (a hit landed and the
        # subject still did not reach or hold combat). Neither is a pass.
        # Requirement 3 (#2945): tell a SETUP failure (``grade_e``: no
        # qualifying hit, or no pending non-player move in the combat
        # observation, so E's arbitration was never exercised) from an
        # ARBITRATION failure (a hit landed with the move pending and the
        # subject still did not reach or hold combat). Neither is a pass.
        print(f"  [E setup: {e_why or 'established'}]")
        e_checks = [("combat reached over a pending non-player move",
                     e_reached),
                    ("combat does NOT hand back to the stale move",
                     not reverted)]
        if e_why is None:
            checks.extend(e_checks)
        else:
            e_setup_failed = True
            print(f"  [E SETUP FAILURE] {e_why}; check E's arbitration was "
                  "not exercised, so its two assertions are NOT graded and "
                  "NOT passed", file=sys.stderr)
        destroy_units(args.port, ue, atk2)

        # --- Check F: the command-time capacity refusal, asserted rather
        # than observed. A's mirror image (#1736): commandPickup gates
        # capacity at COMMAND time (#920), and a refusal deliberately
        # stores no pickupOrder so a position hold survives it (#1216).
        # pickupUtility then scores -inf, leaving the pending move order
        # the correct winner -- which is exactly the outcome check A used
        # to misread as a ladder regression. Staged over capacity here, so
        # it is the capacity path and not commandPickup's separate
        # missing/off-page-item refusal that is under test.
        uf = spawn_acolyte(args.port, args.unit, sx, sy + 6, "F")
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
        seenf, hitf, snapf, lastf = poll_observed(
            args.port, uf, "follow_command", args.seconds)
        print(f"[F refusal-keeps-move] action timeline: {' -> '.join(seenf)}")
        print_obs("F", snapf, lastf)
        checks.append(("a refused pickup leaves the pending move selected",
                       hitf))

        print("\n--- checks ---")
        all_ok = True
        for label, ok in checks:
            print(f"  [{'PASS' if ok else 'FAIL'}] {label}")
            all_ok = all_ok and ok
        if e_setup_failed:
            for label, _ in e_checks:
                print(f"  [SETUP] {label} (check E setup failure: not graded)")
        if not all_ok:
            return 1
        return 3 if e_setup_failed else 0
    finally:
        quit_engine(args.port, proc)


if __name__ == "__main__":
    sys.exit(main())
