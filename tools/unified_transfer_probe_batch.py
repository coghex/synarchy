#!/usr/bin/env python3
"""The batch and widget stages for `tools/unified_transfer_probe.py`
(#2048).

`stage_batch` is D-1: twelve items into room for exactly eight stores
eight, reports the remaining four by count and by the contract's own
`receiver_full` reason, and no single item ever half-moves.

`stage_widget` is requirement 1d, and it is a LEDGER stage rather than a
scenario one: it asserts over the evidence the earlier stages recorded as
each container view opened, because a list instance only exists while its
view is open.
"""
from __future__ import annotations

import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (send, set_paused)
from unified_transfer_probe_support import (Checks, DEF_BATCH,
                                            pane_list_id)
from unified_transfer_probe_world import (ViewLedger, add_one, calm,
                                          check_transfer,
                                          click_widget_center, close_menu,
                                          ep_ids, find_row, find_widget,
                                          menu_labels, outcome_ids,
                                          outcome_states,
                                          right_click_widget_center,
                                          warning_texts)
from unified_transfer_probe_mode_a import (await_session_open,
                                           close_session, create_session)


def stage_batch(chk: Checks, port: int, ids: dict, fp: dict, vp: dict) -> None:
    """D-1: twelve into room for eight stores eight and reports the rest,
    and no single item ever half-moves."""
    acolyte, partial = ids["acolyte"], ids["partial"]
    U_A, B_P = ("unit", acolyte), ("building", partial)
    label = "batch"
    set_paused(port, True)
    calm(port, acolyte)
    before_ids = set(ep_ids(port, U_A, DEF_BATCH))
    for _ in range(12):
        add_one(port, acolyte, DEF_BATCH)
    minted = [i for i in ep_ids(port, U_A, DEF_BATCH)
              if i not in before_ids]
    if not chk.ok(len(minted) == 12 and not before_ids,
                  f"{label}: the acolyte carries exactly twelve {DEF_BATCH} "
                  f"instances and nothing else of that def", f"got {minted!r}"):
        return
    if not chk.ok(not ep_ids(port, B_P, DEF_BATCH),
                  f"{label}: the small hold starts empty of them"):
        return

    if not create_session(chk, port, acolyte, B_P, vp, label, via_menu=False):
        return
    if not await_session_open(chk, port, acolyte, label):
        send(port, "require('scripts.transfer_session').clear(); return 'ok'")
        return

    row = find_row(port, pane_list_id("source"), DEF_BATCH)
    if not chk.ok(bool(row), f"{label}: the merged twelve-instance row is "
                             f"located on the source pane"):
        close_session(chk, port, [acolyte], label)
        return
    chk.ok(sorted(row.get("instanceIds") or []) == sorted(minted),
           f"{label}: the merged row stands for all twelve exact instances",
           f"got {row.get('instanceIds')!r}")
    before_warns = warning_texts(port, acolyte)
    before_check = check_transfer(port, U_A, B_P, DEF_BATCH, minted)
    right_click_widget_center(port, row)
    time.sleep(0.5)
    labels = menu_labels(port)
    chk.ok("Store all" in labels and "Store 1" in labels,
           f"{label}: a merged row offers Store all beside Store 1",
           f"menu labels: {labels!r}")
    entry = find_widget(port, "Store all")
    if not chk.ok(bool(entry), f"{label}: the 'Store all' entry is clickable"):
        close_menu(port)
        close_session(chk, port, [acolyte], label)
        return
    click_widget_center(port, entry)
    time.sleep(1.5)

    stored = ep_ids(port, B_P, DEF_BATCH)
    kept = ep_ids(port, U_A, DEF_BATCH)
    chk.ok(len(stored) == 8,
           f"{label}: eight of the twelve fit and were stored",
           f"got {len(stored)}: {stored!r}")
    chk.ok(len(kept) == 4,
           f"{label}: the four that did not fit are still on the acolyte",
           f"got {len(kept)}: {kept!r}")
    chk.ok(sorted(stored + kept) == sorted(minted)
           and not set(stored) & set(kept),
           f"{label}: every one of the twelve is in EXACTLY ONE endpoint — "
           f"nothing half-moved, nothing was lost and nothing was duplicated",
           f"stored={stored!r} kept={kept!r} minted={sorted(minted)!r}")
    # The remainder is reported by COUNT and by the contract's OWN reason,
    # so read the warning TEXT rather than counting rows that merely
    # mention the verb: "couldn't Store 4 x <item> -- receiver_full" is
    # the whole claim, and a message that reported some other count, or
    # invented a reason, would satisfy a substring count.
    new_warns = [w for w in warning_texts(port, acolyte)
                 if w not in before_warns]
    want = "couldn't Store %d x %s -- receiver_full" % (
        len(minted) - 8, f"Probe UT {DEF_BATCH}")
    chk.ok(len(new_warns) == 1 and want in new_warns[0],
           f"{label}: the remainder is REPORTED exactly once, naming the four "
           f"that did not fit and the contract's own 'receiver_full' reason",
           f"got {new_warns!r}, wanted a single warning containing {want!r}")
    # ...and the contract itself agrees, per instance: the eight that
    # moved are no longer the acolyte's to move, while the four that
    # stayed still are.
    # `completion` is DERIVED from the outcomes here rather than
    # hardcoded, so this asserts the contract is internally consistent
    # about its own summary as well as splitting where D-1 says.
    states = outcome_states(before_check)
    fails = sum(1 for st in states if st.startswith("failed"))
    want_completion = ("all" if fails == 0
                       else "none" if fails == len(states) else "partial")
    chk.ok(isinstance(before_check, dict)
           and before_check.get("accepted") is True
           and outcome_ids(before_check) == [int(i) for i in minted]
           and fails == len(minted) - 8
           and before_check.get("completion") == want_completion,
           f"{label}: the contract's own structured answer already splits "
           f"twelve into eight and four before anything moves, with a "
           f"`completion` that agrees with its own outcomes",
           f"got {before_check!r} (states {states!r})")
    after_stored = check_transfer(port, U_A, B_P, DEF_BATCH, stored)
    after_kept = check_transfer(port, U_A, B_P, DEF_BATCH, kept)
    chk.ok(isinstance(after_stored, dict)
           and outcome_ids(after_stored) == [int(i) for i in stored]
           and all(st == "failed:instance_missing"
                   for st in outcome_states(after_stored)),
           f"{label}: afterwards the contract refuses the eight that moved on "
           f"IDENTITY — they are no longer the acolyte's to offer",
           f"got {after_stored!r}")
    chk.ok(isinstance(after_kept, dict)
           and outcome_ids(after_kept) == [int(i) for i in kept],
           f"{label}: and still names the four that stayed, so the partial "
           f"batch split exactly where the report said it did",
           f"got {after_kept!r}")
    fp["batch"] = {"minted": sorted(minted), "stored": stored, "kept": kept}
    close_session(chk, port, [acolyte], label)


def stage_widget(chk: Checks, ledger: ViewLedger, fp: dict) -> None:
    """Requirement 1d: ONE widget rendered every container view this run
    encountered, asserted from the dumps collected as each one opened."""
    expected = [
        "container window on a building endpoint",
        "container window on a unit endpoint",
        "unit-info inventory section",
        "never-inspected container window",
        "escort source pane",
        "escort destination pane",
    ]
    missing = [name for name in expected if name not in ledger.views]
    chk.ok(not missing,
           "every container view this scenario can produce was actually "
           "opened and recorded", f"never opened: {missing!r}")
    for name, ev in sorted(ledger.views.items()):
        chk.ok(ev["registered"],
               f"the {name} is rendered by the ONE item-list widget (its list "
               f"id names a live instance of it)", f"got {ev!r}")
        chk.ok(ev["allItemList"],
               f"every row the {name} rendered came back through that same "
               f"widget's dump ({ev['rows']} rows)", f"got {ev!r}")
    ids = [ev["listId"] for ev in ledger.views.values() if ev["listId"]]
    chk.ok(len(ids) == len(set(ids)),
           "each view was its own INSTANCE of that widget rather than one "
           "shared list re-pointed at a second endpoint", f"list ids {ids!r}")
    fp["views"] = sorted(ledger.views)
