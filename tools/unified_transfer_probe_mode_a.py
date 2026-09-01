#!/usr/bin/env python3
"""Mode A - the escort-session half of the arc (#1250/#1251), for
`tools/unified_transfer_probe.py` (#2048).

Two things live here, and the distinction is deliberate.

The SESSION LIFECYCLE - `create_session`, `await_session_open`,
`close_session`, `stack_dump`, `session_names` - is this module's PUBLIC
interface. There is exactly one implementation of "open a real escort
session and wait for the pair", and the knowledge, batch and persistence
stages each need one; they call these rather than growing a second copy.

The STAGE BODIES - `mode_a_leg` and `stage_mode_a` - are the six directed
legs themselves: walk first, then choose, committing on the spot from
whichever pane was clicked.
"""
from __future__ import annotations

import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (camera_state, poll_until, send, send_json,
                      set_paused)
from unified_transfer_probe_support import (Checks, LEG_DEFS_A, WINDOW,
                                            pane_list_id)
from unified_transfer_probe_world import (ViewLedger, ai_action,
                                          assert_structured_move, calm,
                                          check_transfer,
                                          click_widget_center, close_menu,
                                          ep_ids, event_total, find_row,
                                          find_widget, focus_entity,
                                          footprint_gap, menu_labels,
                                          retire_leg_item,
                                          right_click_pixel,
                                          right_click_widget_center,
                                          seed_source, send_home, session,
                                          session_phase)


# --------------------------------------------------------------------------
# The escort-session lifecycle: this module's PUBLIC interface
# --------------------------------------------------------------------------
# `knowledge`, `batch` and `save` each open a real escort session too,
# and they open it through exactly these entry points. There is one
# implementation of "create a session, run the world until the escort
# arrives and the pair opens, and tear the whole thing down again",
# because the three ways an open can fail are only distinguishable when
# every caller samples the same phases on the way.
def stack_dump(port: int) -> dict:
    got = send_json(port, f"return {WINDOW}.dump()")
    return got if isinstance(got, dict) else {}


def session_names(port: int, src_uid: int, dst_ep):
    """`(does the live session name exactly these endpoints, the session)`."""
    s = session(port)
    return (isinstance(s, dict)
            and (s.get("source") or {}).get("id") == src_uid
            and (s.get("destination") or {}).get("kind") == dst_ep[0]
            and (s.get("destination") or {}).get("id") == dst_ep[1]), s


def create_session(chk: Checks, port: int, src_uid: int, dst_ep, vp: dict,
                   label: str, via_menu: bool) -> bool:
    """Create a Mode A session — through the REAL right-click "Transfer"
    row when `via_menu`, otherwise through the same
    `transfer_session.create` that row's callback calls.

    The menu route is a REQUIRED observation with a fallback, never a
    silent one: a row that cannot be located, and a row that produced a
    session on some OTHER endpoint, are both recorded as failures here
    and the session is then built directly so the legs below still
    report. A silent fallback would delete these checks instead of
    failing them."""
    send(port, "unit.deselectAll(); return 'ok'")
    send(port, f"return unit.select({src_uid})")
    time.sleep(0.5)
    if via_menu:
        kind = "building" if dst_ep[0] == "building" else "unit"
        pixel = focus_entity(chk, port, kind, dst_ep[1], vp, label)
        if pixel is None:
            via_menu = False   # focus_entity recorded the localization failure
    if via_menu:
        right_click_pixel(port, pixel, vp)
        labels = menu_labels(port)
        entry = find_widget(port, "Transfer")
        if chk.ok(bool(entry),
                  f"{label}: the right-click menu offers 'Transfer'",
                  f"menu labels: {labels!r}"):
            click_widget_center(port, entry)
            time.sleep(0.8)
            named, live = session_names(port, src_uid, dst_ep)
            if chk.ok(named,
                      f"{label}: activating the rendered 'Transfer' row "
                      f"created the session on the exact endpoint identity "
                      f"{dst_ep[0]}:{dst_ep[1]}", f"got {live!r}"):
                return True
            send(port, "require('scripts.transfer_session').clear();"
                       " return 'ok'")
        else:
            close_menu(port)
    made = send(port, "return tostring(require('scripts.transfer_session')"
                      f".create({src_uid}, '{dst_ep[0]}', {dst_ep[1]})"
                      " ~= nil)").strip().strip('"')
    if not chk.ok(made == "true", f"{label}: a Mode A session is created",
                  f"got {made!r}"):
        return False
    named, live = session_names(port, src_uid, dst_ep)
    return chk.ok(named,
                  f"{label}: the session names the exact endpoint identities",
                  f"got {live!r}")


def await_session_open(chk: Checks, port: int, src_uid: int, label: str,
                       seconds: float = 150.0) -> bool:
    """Run the world until the escort arrives and the pair opens.

    Every distinct phase is recorded on the way, because the three ways
    this can fail are not distinguishable from the final state alone:
    stuck on `approaching` is an AI that never won, a phase that reached
    `open` and then vanished is a TEARDOWN (a zoom-band change and a HUD
    hide both clear a session), and never seeing a phase at all is a
    session that was refused."""
    seen: list = []
    stale: list = []
    poses: list = []

    def look():
        # `staleReason` is PUBLIC precisely so a gate can ask the same
        # question the session's own tick asks; sampled alongside the
        # phase, it is what tells "the AI never won" apart from "an
        # endpoint stopped qualifying and the session was torn down".
        got = send(port, "local ts = require('scripts.transfer_session');"
                         " local s = ts.get();"
                         " return (s and s.phase or 'none') .. '/' .."
                         " tostring(s and ts.staleReason() or 'n/a') .. '/'"
                         f" .. tostring(unit.getPose({src_uid}))"
                   ).strip().strip('"')
        phase, _, rest = got.partition("/")
        reason, _, pose = rest.partition("/")
        if pose and pose not in poses:
            poses.append(pose)
        if not seen or seen[-1] != phase:
            seen.append(phase)
        if reason not in ("nil", "n/a", "") and reason not in stale:
            stale.append(reason)
        return phase == "open"

    set_paused(port, False)
    opened = poll_until(seconds, look, interval=0.5)
    set_paused(port, True)
    time.sleep(0.6)
    return chk.ok(bool(opened),
                  f"{label}: the REAL unit AI walks the escort and opens the "
                  f"pair on arrival",
                  f"phases seen {seen!r}, stale reasons seen {stale!r}, "
                  f"escort poses seen {poses!r} (a Collapsed one is a FALL "
                  f"knockdown, which ends the session by rule), escort "
                  f"running {ai_action(port, src_uid)!r}, stack depth "
                  f"{stack_dump(port).get('depth')!r}, camera "
                  f"{camera_state(port)!r}")


# --------------------------------------------------------------------------
# The stage's own bodies: six directed legs across three real sessions
# --------------------------------------------------------------------------
def mode_a_leg(chk: Checks, port: int, fp: dict, key: str, src, dst,
               def_name: str, verb: str, pane: str, iid: int,
               held_uid: int) -> bool:
    """One directed leg inside an open session: a real right-click on the
    real rendered pane row, then the located entry. The direction comes
    from WHICH pane was clicked, so a swapped pair is unrepresentable."""
    label = f"modeA {key}"
    display = f"Probe UT {def_name}"
    other = "Retrieve" if verb == "Store" else "Store"
    rows_id = pane_list_id(pane)
    row = find_row(port, rows_id, def_name)
    if not chk.ok(bool(row),
                  f"{label}: the {def_name} row is located on the rendered "
                  f"{pane} pane"):
        return False
    chk.ok(row.get("instanceIds") == [iid],
           f"{label}: the rendered row stands for exactly instance {iid}",
           f"got {row.get('instanceIds')!r}")

    before_ev = event_total(port, held_uid, "unit_event", display)
    before_check = check_transfer(port, src, dst, def_name, [iid])
    right_click_widget_center(port, row)
    time.sleep(0.5)
    labels = menu_labels(port)
    chk.ok(f"{verb} 1" in labels,
           f"{label}: the {pane} pane's row menu offers '{verb} 1'",
           f"menu labels: {labels!r}")
    chk.ok(not any((l or "").startswith(other) for l in labels),
           f"{label}: and offers no '{other}' — direction comes from WHICH "
           f"pane was clicked", f"menu labels: {labels!r}")
    entry = find_widget(port, f"{verb} 1")
    if not chk.ok(bool(entry), f"{label}: the '{verb} 1' entry is clickable"):
        close_menu(port)
        return False
    click_widget_center(port, entry)
    time.sleep(1.0)

    chk.ok(iid in ep_ids(port, dst, def_name),
           f"{label}: the gesture commits ON THE SPOT — instance {iid} is at "
           f"{dst[0]}:{dst[1]} immediately, with no order and no walk",
           f"destination now holds {ep_ids(port, dst, def_name)!r}")
    chk.ok(iid not in ep_ids(port, src, def_name),
           f"{label}: and left {src[0]}:{src[1]} — the instance is in exactly "
           f"one endpoint")
    chk.ok(event_total(port, held_uid, "unit_event", display) - before_ev == 1,
           f"{label}: the commit reached the player exactly once")
    chk.ok(session_phase(port) == "open",
           f"{label}: the session stays open and repeatable after a commit",
           f"phase {session_phase(port)!r}")
    assert_structured_move(chk, port, label, src, dst, def_name, [iid],
                           before_check, deferred_reach=False)
    fp.setdefault("legs", {})[f"modeA:{key}"] = iid
    retire_leg_item(port, dst, def_name)
    return True


def close_session(chk: Checks, port: int, held: list, label: str) -> None:
    """One dismissal takes both panes AND the session AND every hold."""
    send(port, f"{WINDOW}.popLevel(); return 'ok'")
    time.sleep(0.8)
    chk.ok(session(port) is None,
           f"{label}: closing the level ends the session (one coupled teardown)")
    for uid in held:
        got = send(port, "return tostring(require('scripts.transfer_session')"
                         f".holdsUnit({uid}))").strip().strip('"')
        chk.ok(got == "false", f"{label}: unit {uid} is released by the "
                               f"session ending", f"holdsUnit = {got!r}")


def stage_mode_a(chk: Checks, port: int, ledger: ViewLedger, ids: dict,
                 fp: dict, vp: dict) -> None:
    """The same six directed legs through three real escort sessions."""
    acolyte, mule, hold = ids["acolyte"], ids["technomule"], ids["hold"]
    U_A, U_M, B_H = ("unit", acolyte), ("unit", mule), ("building", hold)

    sessions = [
        # (key, escort uid, destination endpoint, via the real menu?,
        #  walk away first?, legs)
        ("acolyte<->storage", acolyte, B_H, True, True,
         [("acolyte->storage", U_A, B_H, LEG_DEFS_A[0], "Store", "source"),
          ("storage->acolyte", B_H, U_A, LEG_DEFS_A[1], "Retrieve",
           "destination")]),
        ("technomule<->storage", mule, B_H, False, False,
         [("technomule->storage", U_M, B_H, LEG_DEFS_A[2], "Store", "source"),
          ("storage->technomule", B_H, U_M, LEG_DEFS_A[3], "Retrieve",
           "destination")]),
        ("acolyte<->technomule", acolyte, U_M, True, False,
         [("acolyte->technomule", U_A, U_M, LEG_DEFS_A[4], "Store", "source"),
          ("technomule->acolyte", U_M, U_A, LEG_DEFS_A[5], "Retrieve",
           "destination")]),
    ]
    for key, escort, dst_ep, via_menu, walk_away, legs in sessions:
        label = f"modeA {key}"
        set_paused(port, True)
        calm(port, escort)
        if dst_ep[0] == "unit":
            calm(port, dst_ep[1])
            # Get the destination unit off the storage building's own
            # tiles first. The right-click router tries the BUILDING menu
            # before the unit menu, so a unit standing on a hold hands
            # every right-click to the hold — and the previous session
            # left it standing on exactly that.
            send_home(port, dst_ep[1], (ids.get("homes") or {}).get(dst_ep[1]),
                      B_H, gap=3)
        seeded = {}
        for leg_key, src, dst, def_name, verb, pane in legs:
            seed_source(port, src, def_name, acolyte)
            got = ep_ids(port, src, def_name)
            if not chk.ok(len(got) == 1,
                          f"{label}: {src[0]}:{src[1]} holds exactly one "
                          f"{def_name} for the {leg_key} leg", f"got {got!r}"):
                seeded = {}
                break
            seeded[leg_key] = got[0]
        if not seeded:
            continue

        if walk_away:
            # Walk FIRST is the whole of Mode A, so at least one session
            # has to start out of reach and get there under its own AI.
            send_home(port, escort, (ids.get("homes") or {}).get(escort),
                      dst_ep, gap=2, seconds=90.0)
            gap = footprint_gap(port, ("unit", escort), dst_ep)
            chk.ok(gap is not None and gap > 1,
                   f"{label}: the escort starts OUT of the contract's reach "
                   f"(footprint Chebyshev {gap}), so the session has a real "
                   f"walk to make", f"gap={gap!r}")

        if not create_session(chk, port, escort, dst_ep, vp, label, via_menu):
            send(port, "require('scripts.transfer_session').clear(); return 'ok'")
            continue
        if not await_session_open(chk, port, escort, label):
            send(port, "require('scripts.transfer_session').clear(); return 'ok'")
            continue

        d = stack_dump(port)
        lv = (d.get("levels") or [{}])[0]
        chk.ok(d.get("depth") == 1 and lv.get("kind") == "escort"
               and lv.get("paneCount") == 2,
               f"{label}: the pair is ONE stack level of the escort kind "
               f"owning two panes", f"got {d!r}")
        ledger.record(port, "escort source pane", pane_list_id("source"))
        ledger.record(port, "escort destination pane",
                      pane_list_id("destination"))

        held = [escort]
        if dst_ep[0] == "unit":
            held.append(dst_ep[1])
            roles = {uid: send(port, "return tostring(require("
                                     "'scripts.transfer_session')"
                                     f".roleOf({uid}))").strip().strip('"')
                     for uid in held}
            chk.ok(roles.get(escort) == "source"
                   and roles.get(dst_ep[1]) == "target",
                   f"{label}: unit-to-unit holds BOTH ends — the escort as "
                   f"'source', the destination as 'target'", f"got {roles!r}")

        for leg_key, src, dst, def_name, verb, pane in legs:
            mode_a_leg(chk, port, fp, leg_key, src, dst, def_name, verb, pane,
                       seeded[leg_key], escort)
        close_session(chk, port, held, label)
