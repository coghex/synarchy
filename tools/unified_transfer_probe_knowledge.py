#!/usr/bin/env python3
"""The knowledge stage - D-2's remembered-never-live contract - for
`tools/unified_transfer_probe.py` (#2048).

Four observations in one ordered pass, and the order is what makes them
observations rather than assertions about this probe's own writes: a
container nothing has ever touched reads unknown and reveals nothing on
being opened; proximity alone never reveals; a NON-player withdrawal
mutates storage without revealing, which is how the record goes
genuinely stale; and a Mode A session OPENING on it is what refreshes it.
"""
from __future__ import annotations

import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (send, send_json, set_paused)
from unified_transfer_probe_support import (CARGO_LIST_ID, Checks,
                                            DEF_STALE)
from unified_transfer_probe_world import (ViewLedger, calm,
                                          click_widget_center,
                                          close_window, footprint_gap,
                                          item_rows, knowledge,
                                          live_storage_ids, open_window,
                                          park_beside, remembered_ids,
                                          tab_boxes)
from unified_transfer_probe_mode_a import (await_session_open,
                                           close_session, create_session)


def stage_knowledge(chk: Checks, port: int, ledger: ViewLedger, ids: dict,
                    fp: dict, vp: dict) -> None:
    """D-2: contents are genuinely stale, and only an INTERACTION
    refreshes them."""
    unseen, stale = ids["unseen"], ids["stale"]
    set_paused(port, True)

    # -- never inspected.
    k = knowledge(port, unseen)
    if not chk.ok(k.get("state") == "unknown",
                  "a container nothing has ever interacted with reads as "
                  "genuinely never-inspected", f"got {k!r}"):
        return
    cap = k.get("capacity")
    chk.ok(isinstance(cap, (int, float)) and cap > 0,
           "its capacity is still LIVE and positive even with no record at all",
           f"got {cap!r}")
    if open_window(chk, port, ("building", unseen), "knowledge"):
        ledger.record(port, "never-inspected container window", CARGO_LIST_ID)
        sub = send_json(port, "local s = require('scripts.cargo_inventory_panel')"
                              ".getLevel(1) or {}; local l ="
                              " require('scripts.ui.label');"
                              " return {subtitle = s.subtitleId and"
                              " l.getText(s.subtitleId)}")
        want = "Storage: unknown / %.2f kg" % float(cap)
        chk.ok(isinstance(sub, dict) and sub.get("subtitle") == want,
               "its header reads the stored weight as UNKNOWN while still "
               "showing the live capacity",
               f"got {(sub or {}).get('subtitle')!r} want {want!r}")
        chk.ok(not item_rows(port, CARGO_LIST_ID),
               "a never-inspected container renders no rows — it is not an "
               "empty one")
        time.sleep(1.5)
        for t in tab_boxes(port, CARGO_LIST_ID):
            click_widget_center(port, t)
            time.sleep(0.3)
        after = knowledge(port, unseen)
        chk.ok(after.get("state") == "unknown"
               and after.get("revealedAt") is None,
               "opening the window and interacting with it reveals NOTHING",
               f"got {after!r}")
        close_window(port)
    # Every observation this fixture exists for has now been made, and it
    # is the world's only construction site: `build_nearby` scans 30
    # tiles for a Constructing building with work left, so leaving it
    # standing would pull acolytes away from the very walks the rest of
    # this run measures — and would eventually finish it, which SEEDS a
    # knowledge record and destroys the state it was built to supply.
    send(port, f"return building.destroy({unseen})")
    time.sleep(0.5)
    chk.ok(send(port, f"return building.getActivity({unseen})"
                ).strip().strip('"') in ("", "nil", "null"),
           "the never-inspected fixture is retired once it has been observed, "
           "so no construction site competes with the escorts below",
           f"activity {send(port, f'return building.getActivity({unseen})')!r}")

    # -- a known container, and what does NOT refresh it.
    k = knowledge(port, stale)
    r0 = k.get("revealedAt")
    stocked = remembered_ids(port, stale, DEF_STALE)
    if not chk.ok(k.get("state") == "known" and isinstance(r0, (int, float))
                  and len(stocked) == 2,
                  "the stocked container is remembered with both instances and "
                  "an observation time", f"got {k!r}"):
        return

    watcher = ids["watcher"]
    parked = park_beside(port, watcher, ("building", stale))
    gap0 = footprint_gap(port, ("unit", watcher), ("building", stale))
    if chk.ok(bool(parked) and gap0 is not None and gap0 <= 1,
              f"a player unit is standing right beside it (footprint "
              f"Chebyshev {gap0})", f"gap={gap0!r}"):
        set_paused(port, False)
        time.sleep(8.0)
        set_paused(port, True)
        time.sleep(0.4)
        gap1 = footprint_gap(port, ("unit", watcher), ("building", stale))
        chk.ok(gap1 is not None and gap1 <= 3,
               f"and stayed in its immediate vicinity for the whole interval "
               f"(footprint Chebyshev {gap0} -> {gap1})", f"gap={gap1!r}")
        chk.ok(knowledge(port, stale).get("revealedAt") == r0
               and remembered_ids(port, stale, DEF_STALE) == stocked,
               "PROXIMITY ALONE never reveals: eight seconds of a player unit "
               "standing beside it changes neither the observation time nor "
               "the remembered contents",
               f"got {knowledge(port, stale)!r}")

    # -- and how the record goes genuinely stale. A non-player unit's
    #    withdrawal mutates storage without revealing, because every
    #    unit-driven reveal is gated on isPlayerCommandable — so this is
    #    a real divergence rather than one manufactured by a test verb.
    took = send(port, f"return unit.withdrawFromCargo({ids['wildlife']},"
                      f" {stale}, '{DEF_STALE}')").strip().strip('"')
    chk.ok(took == "true", "a non-player unit withdraws one instance",
           f"got {took!r}")
    live = live_storage_ids(port, stale, DEF_STALE)
    remembered = remembered_ids(port, stale, DEF_STALE)
    chk.ok(len(live) == 1 and remembered == stocked
           and knowledge(port, stale).get("revealedAt") == r0,
           "the player's picture is now GENUINELY STALE: storage holds one "
           "instance while the record still remembers both, unchanged",
           f"live={live!r} remembered={remembered!r}")

    # -- the Mode A open is the interaction that refreshes it.
    reveal_uid = ids["reveal"]
    calm(port, reveal_uid)
    escort_info = send_json(port, f"return unit.getInfo({reveal_uid})")
    anim = (escort_info or {}).get("currentAnim") if isinstance(
        escort_info, dict) else None
    if not chk.ok(isinstance(escort_info, dict)
                  and not str(anim or "").startswith("injured_"),
                  "the escort is alive and unhurt before the session that "
                  "measures the reveal — an incapacitated source ends a "
                  "session by rule, which would read as a reveal bug",
                  f"got {escort_info!r}"):
        return
    if not create_session(chk, port, reveal_uid, ("building", stale), vp,
                          "knowledge Mode A reveal", via_menu=False):
        return
    if not await_session_open(chk, port, reveal_uid, "knowledge Mode A reveal",
                              seconds=60.0):
        send(port, "require('scripts.transfer_session').clear(); return 'ok'")
        return
    k2 = knowledge(port, stale)
    chk.ok(isinstance(k2.get("revealedAt"), (int, float))
           and k2["revealedAt"] > float(r0),
           "a Mode A session OPENING on the container is what refreshes it — "
           "a fresh observation time", f"got {k2!r} (was {r0!r})")
    chk.ok(remembered_ids(port, stale, DEF_STALE) == live,
           "and the refreshed record now agrees with live storage, the "
           "withdrawn instance gone",
           f"remembered={remembered_ids(port, stale, DEF_STALE)!r} live={live!r}")
    fp["knowledge"] = {"stocked": stocked, "afterWithdraw": live}
    close_session(chk, port, [reveal_uid], "knowledge Mode A reveal")
