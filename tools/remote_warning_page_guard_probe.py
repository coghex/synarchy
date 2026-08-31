#!/usr/bin/env python3
"""Remote-warning cross-page revalidation guard probe (#844).

#779's remote-settlement confirmation (`scripts/build_tool_remote_warning.lua`)
re-validates its saved (defName, gx, gy) against `building.canPlaceAt` /
`commitStartingPlacement` when the player clicks "Establish Here" — but
both of those resolve whichever world page is active AT CALL TIME
(`activeWorldPage`), not whichever was active when the modal opened. If
the active page changes while the modal is up (`world.show`), confirming
used to validate and spawn against the NEW page's terrain/locations
instead of rejecting the stale confirmation.

#844 fixes this: `open()` now captures `world.getActiveWorldId()` into
`pending.worldId`, and `establishHere()` compares it against the current
active id BEFORE calling `canPlaceAt`/`commitStartingPlacement`, treating
a mismatch exactly like a failed revalidation (no spawn, a
`revalidationRejected` outcome) with a `reason` that distinguishes an
active-world change from an ordinary invalid-tile rejection.

No GPU is needed: the full modal (panel/label/box) creates fine under
`--headless` even with placeholder (0) texture/font handles, since
nothing is ever drawn — only `UI.newBox`/`engine.getTextWidth` etc. are
exercised, all pure state/lookup calls. Two flat `world.initArena` pages
stand in for "two real world pages" (no worldgen needed — an arena has
no placed locations, so any position is trivially a valid, remote
starting-portal site).

Checks:
  1. Opening the confirmation on page A captures state and presents it
     (a "presented" outcome, `isOpen()` true).
  2. Switching the active world to page B, then calling `establishHere()`,
     closes the modal, spawns nothing, and records exactly one
     `revalidationRejected` outcome with reason "active world changed" —
     no `buildTool.commitPlacement` outcome, no portal in
     `building.list()` on either page.
  3. Sanity (regression guard): the happy path is unaffected — opening
     and establishing on the SAME page still spawns the portal
     ("accepted"), and `chooseAnotherSite()` still cancels normally
     (requirement 3 — unchanged).

This probe implements the shared `probe-result/v1` contract: `--describe`
prints its ordered stable checks without booting an engine, and a harnessed
run writes structured events while a standalone run keeps its existing
human-readable result lines and exit status.

Usage: python3 tools/remote_warning_page_guard_probe.py [--port 9421]
       python3 tools/remote_warning_page_guard_probe.py --describe
"""
from __future__ import annotations

import argparse
import glob
import sys

import probe_protocol
from probelib import boot, quit_engine, send, send_json, poll_until

LOG = "/tmp/remote_warning_page_guard_probe_engine.log"
LOG_NAME = "remote_warning_page_guard_probe_engine.log"
PROBE_KEY = "remote_warning_page_guard"
PORTAL = "acolyte_portal"
PAGE_A = "rw_page_a"
PAGE_B = "rw_page_b"
# A third, never-before-shown page for the same-page sanity checks below —
# world.show only PREPENDS a page to the (multi-world-visible) wmVisible
# list when it isn't already an element; re-showing page A after B has
# taken over would be a no-op (A stays second, B stays head), so the
# happy-path / cancel-path sanity checks use a page that's shown exactly
# once instead of re-visiting page A.
PAGE_C = "rw_page_c"

CHECKS = [
    ("page_a_active", "page A becomes active"),
    ("remote_position_valid",
     "the fresh-arena position is valid and remote"),
    ("modal_open_on_page_a", "open presents the modal on page A"),
    ("presented_outcome", "open records one presented outcome"),
    ("page_b_active", "page B becomes active while the modal is open"),
    ("modal_closed_after_switch",
     "establishHere closes the modal after the page switch"),
    ("revalidation_rejected_once",
     "establishHere records exactly one revalidation rejection"),
    ("active_world_change_reason",
     "the rejection identifies the active-world change"),
    ("confirmation_outcome",
     "the rejected confirmation still records a confirmed outcome"),
    ("commit_outcome_absent",
     "the rejected confirmation records no placement commit"),
    ("cross_page_spawn_absent",
     "the rejected confirmation spawns no portal on either page"),
    ("page_c_active", "page C becomes active for the control paths"),
    ("same_page_modal_closed",
     "same-page establishHere closes the modal"),
    ("same_page_commit_accepted",
     "same-page establishHere records one accepted placement"),
    ("single_portal_after_accept",
     "the same-page happy path creates exactly one portal"),
    ("cancel_modal_closed", "chooseAnotherSite closes the modal"),
    ("cancel_outcome", "chooseAnotherSite records one canceled outcome"),
    ("single_portal_after_cancel",
     "the cancel path creates no additional portal"),
]

DESCRIPTOR = probe_protocol.build_descriptor(PROBE_KEY, CHECKS)

failures: list[str] = []


def check(rep, check_id: str, name: str, ok: bool,
          detail: str = "") -> bool:
    human = name + (f" — {detail}" if detail and not ok else "")
    rep.check(check_id, ok, human,
              {"observed": detail} if detail else None)
    if not ok:
        failures.append(name if not detail else f"{name} — {detail}")
    return ok


def load_defs(port: int) -> None:
    for pattern, fn in [
        ("data/materials/*.yaml", "engine.loadMaterialYaml"),
        ("data/buildings/*.yaml", "engine.loadBuildingYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")


def init_arena(port: int, name: str) -> None:
    send(port, f"world.initArena('{name}')", expect_result=False)
    send(port, "return world.waitForInit(60)", timeout=70)


def show_and_wait(port: int, name: str, seconds: float = 10.0) -> bool:
    send(port, f"world.show('{name}')", expect_result=False)
    return poll_until(
        seconds,
        lambda: send(port, "return world.getActiveWorldId()").strip('"') == name,
    ) is not None


def drain(port: int):
    got = send_json(port, "return debug.drainActionOutcomes()")
    return got if isinstance(got, list) else []


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9421)
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


def _run(port: int, rep) -> int:
    failures.clear()

    proc = boot(port, log=rep.engine_log_path(LOG_NAME, LOG),
                args=rep.engine_args())
    try:
        load_defs(port)
        init_arena(port, PAGE_A)
        init_arena(port, PAGE_B)
        init_arena(port, PAGE_C)
        check(rep, "page_a_active", "page A becomes active",
              show_and_wait(port, PAGE_A))

        valid = send_json(port, f"local v = building.canPlaceAt('{PORTAL}', 0, 0); return v")
        remote = send_json(port, f"local r = building.remoteCheck('{PORTAL}', 0, 0); return r")
        check(rep, "remote_position_valid",
              "(0,0) on a fresh arena is a valid, remote starting position",
              valid is True and remote is True,
              f"canPlaceAt={valid}, remoteCheck={remote}")

        send(port, "local rw = require('scripts.build_tool_remote_warning'); "
                   "rw.init(0, 0, 0, 1280, 720); return 'ok'")
        drain(port)  # clear startup noise

        # -- 1. open the confirmation on page A --
        opened = send_json(
            port,
            f"local rw = require('scripts.build_tool_remote_warning'); "
            f"rw.open('{PORTAL}', 0, 0, nil, 128); return rw.isOpen()")
        check(rep, "modal_open_on_page_a",
              "open() presents the modal (isOpen() true)", opened is True)

        presented = drain(port)
        check(rep, "presented_outcome",
              "open() records a 'presented' outcome",
              len(presented) == 1 and presented[0].get("outcome") == "presented",
              str(presented))

        # -- 2. switch the active world to page B while it's open --
        check(rep, "page_b_active",
              "page B becomes active while the modal is open",
              show_and_wait(port, PAGE_B))

        closed = send_json(
            port,
            "local rw = require('scripts.build_tool_remote_warning'); "
            "rw.establishHere(); return rw.isOpen()")
        check(rep, "modal_closed_after_switch",
              "establishHere() closes the modal despite the page switch",
              closed is False)

        rejected_outcomes = drain(port)
        confirmed = [o for o in rejected_outcomes if o.get("outcome") == "confirmed"]
        rejected = [o for o in rejected_outcomes
                    if o.get("outcome") == "revalidationRejected"]
        accepted = [o for o in rejected_outcomes
                    if o.get("kind") == "buildTool.commitPlacement"]
        check(rep, "revalidation_rejected_once",
              "establishHere() records exactly one revalidationRejected",
              len(rejected) == 1, str(rejected_outcomes))
        check(rep, "active_world_change_reason",
              "the rejection reason distinguishes an active-world change",
              bool(rejected) and rejected[0].get("reason") == "active world changed",
              str(rejected_outcomes))
        check(rep, "confirmation_outcome",
              "a 'confirmed' outcome is still recorded (matches the ordinary "
              "revalidation-failure contract)",
              len(confirmed) == 1, str(rejected_outcomes))
        check(rep, "commit_outcome_absent",
              "no buildTool.commitPlacement outcome was recorded",
              len(accepted) == 0, str(rejected_outcomes))

        listing_after_reject = send(port, "return building.list()")
        check(rep, "cross_page_spawn_absent",
              "no portal was spawned on either page",
              PORTAL not in listing_after_reject, listing_after_reject)

        # -- 3. sanity: the happy path (same page throughout) is unaffected --
        check(rep, "page_c_active", "page C becomes active",
              show_and_wait(port, PAGE_C))
        drain(port)  # clear noise

        send(port,
             f"local rw = require('scripts.build_tool_remote_warning'); "
             f"rw.open('{PORTAL}', 4, 4, nil, 128); return rw.isOpen()")
        happy_closed = send_json(
            port,
            "local rw = require('scripts.build_tool_remote_warning'); "
            "rw.establishHere(); return rw.isOpen()")
        check(rep, "same_page_modal_closed",
              "same-page establishHere() closes the modal",
              happy_closed is False)

        happy_outcomes = drain(port)
        happy_accepted = [o for o in happy_outcomes
                          if o.get("kind") == "buildTool.commitPlacement"
                          and o.get("outcome") == "accepted"]
        check(rep, "same_page_commit_accepted",
              "same-page establishHere() still spawns the portal (accepted)",
              len(happy_accepted) == 1, str(happy_outcomes))

        listing_after_accept = send(port, "return building.list()")
        check(rep, "single_portal_after_accept",
              "exactly one portal now exists (the same-page happy path)",
              listing_after_accept.count(PORTAL) == 1, listing_after_accept)

        # -- requirement 3: chooseAnotherSite()/Escape are unaffected --
        send(port,
             f"local rw = require('scripts.build_tool_remote_warning'); "
             f"rw.open('{PORTAL}', -4, -4, nil, 128); return rw.isOpen()")
        drain(port)  # clear the 'presented' noise
        cancel_closed = send_json(
            port,
            "local rw = require('scripts.build_tool_remote_warning'); "
            "rw.chooseAnotherSite(); return rw.isOpen()")
        check(rep, "cancel_modal_closed",
              "chooseAnotherSite() still cancels normally",
              cancel_closed is False)
        cancel_outcomes = drain(port)
        check(rep, "cancel_outcome",
              "chooseAnotherSite() records 'canceled', spawning nothing",
              len(cancel_outcomes) == 1
              and cancel_outcomes[0].get("outcome") == "canceled",
              str(cancel_outcomes))
        listing_after_cancel = send(port, "return building.list()")
        check(rep, "single_portal_after_cancel",
              "cancel path still spawned nothing new (still exactly one portal)",
              listing_after_cancel.count(PORTAL) == 1, listing_after_cancel)

    finally:
        quit_engine(port, proc)

    if failures:
        rep.note(f"\n{len(failures)} check(s) FAILED:")
        for f in failures:
            rep.note(f"  - {f}")
        return 1
    rep.note("\nAll checks passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
