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

from .constants import (ACOLYTE_DEF, EXPECTED_COMPLETED, LOG_A, LOG_B, PAGE,
                        SLOT)
from .harness import (Checks, ExpeditionState, StageAbort,
                      check_ai_tick_clean)
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
    done, status = wait_save_complete(port, rid)
    chk.ok(done, f"save {rid} reached SaveCaptureComplete ({status})")
    check_ai_tick_clean(chk, LOG_A, "engine A")


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
        raise StageAbort("the save did not load")
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
    knew = poll_until(30.0, lambda: key in known_locations(port, prepared),
                      interval=1.0)
    chk.ok(bool(knew),
           f"the expedition unit still knows that exact (page, instance) "
           f"pair after the restart ({key} in "
           f"{sorted(known_locations(port, prepared))})")

    completed, _checked = poll_until(
        45.0, lambda: (lambda p: p if p[0] else None)(progress(port)),
        interval=1.0) or progress(port)
    chk.ok(completed == EXPECTED_COMPLETED,
           f"the completed objective set survives the reload exactly "
           f"({sorted(completed)})")

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
