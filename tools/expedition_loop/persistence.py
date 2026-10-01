#!/usr/bin/env python3
"""[save] and [load] — capture it, then prove it in a fresh process
(#2092).

`save` runs at the end of engine A and captures everything the earlier
stages created through the real save barrier. `load` runs in engine B —
a genuinely fresh process that has generated nothing, walked nothing and
picked nothing up — and re-checks every durable identity off the disk:
the location instance and its compound clearance predicate, #917's
guaranteed item down to its physical instance id, the traveller's
per-unit location knowledge, the completed objective set, and the
recovered item's own properties and storage ownership.

One owner because the two stages are the two ends of one round trip: the
save handoff (`SLOT`, and the identities on `ExpeditionState`) is the
only thing that crosses between the processes, and there is exactly one
place that writes it and one that reads it.

Neither function boots or quits an engine — the facade owns both
lifecycles. `enter_load` exists because the pre-split probe entered the
`load` stage BEFORE engine B was launched, so a boot that dies is
attributed to `load` rather than to whichever stage happened to be
current; the facade calls it in that same position.
"""
from __future__ import annotations

import time

from probelib import (capture_request_id, send, send_json, poll_until,
                      wait_load_published, wait_save_complete)

from .constants import (ACOLYTE_DEF, LOG_A, LOG_B, PAGE,
                        REQUIRED_RELOAD_COMPLETED, SLOT, TRIP_OBJECTIVES)
from .day_budget import safe
from .harness import (Checks, ExpeditionState, StageAbort,
                      check_ai_tick_clean)
from .notices import latch_passes
from .readers import (clearance_events, find_instance, instance_by_id,
                      inventory, known_locations, progress, properties, roster,
                      significant_rows)


def save(chk: Checks, st: ExpeditionState) -> None:
    """Capture the finished expedition, through the real save barrier."""
    port = st.port

    chk.enter("save", "capture the finished expedition")
    saved = send(port, f"return engine.saveWorld('{PAGE}', '{SLOT}')")
    chk.ok(saved.strip() == "true", f"engine.saveWorld accepted ({saved!r})")
    rid = capture_request_id(port, "return engine.getSaveStatus()")
    t0 = st.day.now()
    done, status = wait_save_complete(port, rid)
    if not chk.ok(done, f"save {rid} reached SaveCaptureComplete ({status})"):
        st.day.timed_out("the save capture (the save barrier, not a unit, "
                         "is what is awaited)", None, t0)
    check_ai_tick_clean(chk, LOG_A, "engine A")


def load_occupied(chk: Checks, st: ExpeditionState) -> None:
    """The occupied ruin (#2640), re-checked off the disk under the SAME
    `(page, instance id)`: cleared, its persisted roll and membership
    intact with every member still dead and nobody respawned, its
    guaranteed item's identity, provenance and latch, its spent notice,
    and that item in colony storage."""
    port, occ_id, phys = st.port, st.occ_id, st.occ_sig_phys
    inst = instance_by_id(port, PAGE, occ_id) or {}
    enc = inst.get("encounter") or {}
    members = sorted(int(o["uid"]) for o in enc.get("occupants") or [])
    chk.ok(inst.get("lifecycle") == "cleared"
           and inst.get("contents_spawned") is True
           and inst.get("clearance_satisfied") is True
           and int(inst.get("gx", 0)) == int(st.occ["gx"])
           and int(inst.get("gy", 0)) == int(st.occ["gy"]),
           f"the occupied ruin comes back as {PAGE}#{occ_id} at the same "
           f"anchor, 'cleared', with contents_spawned "
           f"(lifecycle {inst.get('lifecycle')!r}, contents_spawned "
           f"{inst.get('contents_spawned')!r})")
    chk.ok(int(enc.get("rolled_count", -1)) == st.occ_rolled
           and members == sorted(st.occ_members)
           and enc.get("cleared") is True,
           f"its persisted encounter roll and occupant membership are "
           f"unchanged ({enc.get('rolled_count')!r}, {members} vs "
           f"{sorted(st.occ_members)}), encounter still cleared")
    # Each ORIGINAL occupant, by uid: it must still exist — a dead unit
    # persists as a corpse — and be dead. A missing uid is a dropped
    # unit, not a dead one.
    time.sleep(3.0)
    state = {u: (send(port, f"return tostring(unit.exists({u}))"),
                 send(port, f"return tostring(unit.getPose({u}))"))
             for u in sorted(st.occ_members)}
    respawned = [o for o in (instance_by_id(port, PAGE, occ_id) or {})
                 .get("encounter", {}).get("occupants", [])
                 if int(o["uid"]) not in st.occ_members]
    chk.ok(all(s_ == ("true", "dead") for s_ in state.values())
           and not respawned,
           f"every original occupant uid still exists and is still dead, and "
           f"no new member has appeared (uid -> (exists, pose): {state}; "
           f"new members {respawned})")
    rows = significant_rows(port, occ_id)
    chk.ok(len(rows) == 1 and rows[0].get("item_instance_id") == phys
           and rows[0].get("taken") is True
           and rows[0].get("item") == st.occ_sig_def,
           f"its guaranteed item's identity, provenance and taken latch "
           f"survive the restart ({rows})")
    chk.ok(inst.get("clear_event_emitted") is True,
           f"its one clearance notice is recorded as spent "
           f"(clear_event_emitted={inst.get('clear_event_emitted')!r})")
    stored = send_json(port, f"return building.getStorage({st.storage_bid})")
    hit = find_instance(stored if isinstance(stored, list) else [], phys)
    chk.ok(hit is not None
           and hit.get("defName") == st.occ_sig_def,
           f"and that item is still colony stock under its own instance id "
           f"and definition ({phys}, {(hit or {}).get('defName')!r})")


def enter_load(chk: Checks) -> None:
    """Open the `load` stage BEFORE engine B is launched.

    `probelib.boot` reports an engine that dies before READY by calling
    `sys.exit()`, and the facade records that against `chk.stage`.
    Entering here is what makes a failed engine-B boot report as a
    `load` failure, exactly as it did pre-split.
    """
    chk.enter("load", "a fresh process reloads the finished expedition")


def load(chk: Checks, st: ExpeditionState) -> None:
    """A fresh process reloads it, and every durable identity holds."""
    port = st.port
    prepared, storage_bid = st.prepared, st.storage_bid
    ruin, ruin_id, sig_phys = st.ruin, st.ruin_id, st.sig_phys
    recovered = st.recovered

    send(port, f"engine.loadSave('{SLOT}'); return 'queued'")
    published, status = wait_load_published(port, 240)
    if not chk.ok(published, f"the save loads and publishes ({status})"):
        # Not read against the budget: the fresh process's game time is
        # its own until a publish installs the save's.
        st.day.timed_out("the load publish (the load pipeline, not a "
                         "unit, is what is awaited)")
        raise StageAbort("the save did not load")
    # The `load` stage's boundary reading, now that a loaded session
    # exists: its game time is the save's, so engine A's deadline holds.
    st.day.loaded(chk)

    # RESTORED, not recomputed — read straight after the publish, before
    # the session is unpaused. The latch recorder `bootstrap` installed
    # in this fresh engine logs every latch an evaluation pass writes;
    # the save component's apply() writes the set directly and never
    # passes through it. So a required latch that is present here and
    # ABSENT from this engine's record came off the disk. It matters
    # most for Secure, whose predicate (a living acolyte CARRYING a taken
    # item) is false in this world, where both guaranteed items sit in
    # colony storage.
    restored, _ = progress(port)
    recomputed = sorted(set(latch_passes(port)) & REQUIRED_RELOAD_COMPLETED)
    chk.ok(REQUIRED_RELOAD_COMPLETED <= restored and not recomputed,
           f"the tutorial latches come back FROM THE SAVE: every required "
           f"preparation and trip latch is present straight after the "
           f"publish, and none of them was written by an evaluation pass "
           f"in this process (recomputed: {recomputed or 'none'}; restored "
           f"{sorted(restored)})")
    send(port, f"world.show('{PAGE}'); return 'ok'")
    # Loads come up paused by design. scripts/tutorial_eval.lua
    # is deliberately not pause-gated, but scripts/unit_ai.lua
    # is — and the withdrawal below is a real unit action, so the
    # session has to be running for it, exactly as it would be
    # for a player resuming a save.
    send(port, "engine.setPaused(false); return 'ok'")

    inst = instance_by_id(port, PAGE, int(ruin["instance_id"]))
    chk.ok(isinstance(inst, dict)
           and inst.get("lifecycle") in ("active", "cleared"),
           f"the SAME page and location-instance id retains its visible "
           f"encounter lifecycle "
           f"after the restart ({PAGE}#{ruin['instance_id']} -> "
           f"{(inst or {}).get('lifecycle')!r})")
    chk.ok(isinstance(inst, dict) and inst.get("contents_spawned") is True,
           f"and its contents are still recorded as spawned exactly once "
           f"(contents_spawned={(inst or {}).get('contents_spawned')!r})")

    # #917: the whole durable half of the significant-contents
    # contract, re-checked in a FRESH PROCESS — identity,
    # provenance, the taken latch, the compound predicate, and
    # the one-shot notice. Nothing here was written by this
    # engine: it all came off the disk.
    rows_after = significant_rows(port, ruin_id)
    chk.ok(len(rows_after) == 1
           and rows_after[0].get("item_instance_id") == sig_phys
           and rows_after[0].get("taken") is True,
           f"the guaranteed item's identity, provenance and taken latch "
           f"survive the restart ({rows_after})")
    chk.ok(isinstance(inst, dict)
           and inst.get("lifecycle") == "cleared"
           and inst.get("clearance_satisfied") is True,
           f"the ruin is still CLEARED, with its compound predicate still "
           f"satisfied ({(inst or {}).get('lifecycle')!r})")
    # The notice is a spent one-shot, and player events are
    # per-session and never saved — so a reloaded, already-cleared
    # ruin must announce nothing at all, however long the
    # discovery tick polls it.
    chk.ok(isinstance(inst, dict)
           and inst.get("clear_event_emitted") is True,
           f"its one clearance notice is recorded as already spent "
           f"(clear_event_emitted="
           f"{(inst or {}).get('clear_event_emitted')!r})")
    time.sleep(5.0)
    repeat = clearance_events(port)
    chk.ok(not repeat,
           f"and the reload re-announces nothing "
           f"({[e.get('text') for e in repeat]})")
    # The item itself is somewhere else entirely now, which is
    # explicitly allowed: the latch records that the ruin was
    # looted, not where the loot went.
    stored_now = send_json(port,
                           f"return building.getStorage({storage_bid})")
    chk.ok(find_instance(
               stored_now if isinstance(stored_now, list) else [],
               sig_phys) is not None,
           f"the guaranteed item is still in colony storage as that same "
           f"physical instance ({sig_phys})")
    chk.ok(isinstance(inst, dict)
           and int(inst.get("gx", 0)) == int(ruin["gx"])
           and int(inst.get("gy", 0)) == int(ruin["gy"])
           and inst.get("id") == ruin.get("id"),
           f"with its definition and anchor unchanged "
           f"({(inst or {}).get('id')!r} at "
           f"({(inst or {}).get('gx')},{(inst or {}).get('gy')}))")

    key = f"{PAGE}#{ruin['instance_id']}"
    t0 = st.day.now()
    knew = poll_until(30.0, lambda: key in known_locations(port, prepared),
                      interval=1.0)
    if not chk.ok(bool(knew),
                  f"the expedition unit still knows that exact (page, "
                  f"instance) pair after the restart ({key} in "
                  f"{safe(lambda: sorted(known_locations(port, prepared)))})"):
        st.day.timed_out("the restored location knowledge", [prepared], t0)

    t0 = st.day.now()
    completed, _checked = poll_until(
        45.0, lambda: (lambda p: p if p[0] else None)(progress(port)),
        interval=1.0) or safe(lambda: progress(port), (set(), set()))
    if not chk.ok(REQUIRED_RELOAD_COMPLETED <= completed,
                  f"all required preparation completions and the four trip "
                  f"objectives {TRIP_OBJECTIVES} still hold once the "
                  f"session is running again ({sorted(completed)})"):
        st.day.timed_out("the restored objectives (the tutorial state, not "
                         "a unit, is what is awaited)", None, t0)

    load_occupied(chk, st)

    stored = send_json(port, f"return building.getStorage({storage_bid})")
    stored = stored if isinstance(stored, list) else []
    match = find_instance(stored, recovered["instanceId"])
    chk.ok(match is not None,
           f"the recovered item is still owned by colony storage "
           f"(bid {storage_bid}, instance {recovered['instanceId']})")
    chk.ok(match is not None
           and match.get("defName") == recovered.get("defName"),
           f"with its definition intact "
           f"({(match or {}).get('defName')!r})")
    chk.ok(match is not None
           and properties(match) == properties(recovered),
           f"and every mutable property intact "
           f"({properties(match)} vs {properties(recovered)})")

    # "invest", for this deferred-reward slice: the recovered
    # loot is a first-class colony asset a DIFFERENT colonist can
    # draw on, indistinguishable from a locally produced one.
    party = roster(port)
    others = [u for u in party.get(ACOLYTE_DEF, []) if u != prepared]
    user = others[0] if others else -1
    ok = send(port, f"return unit.withdrawFromCargo({user},{storage_bid},"
                    f"'{recovered['defName']}',{recovered['instanceId']})")
    held = find_instance(inventory(port, user), recovered["instanceId"])
    chk.ok(ok.strip() == "true" and held is not None
           and properties(held) == properties(recovered),
           f"a different colonist ({user}) draws that exact instance back "
           f"out of colony storage and holds it unchanged — the recovered "
           f"item is usable colony stock (returned {ok!r}, {properties(held)})")
    check_ai_tick_clean(chk, LOG_B, "engine B")
