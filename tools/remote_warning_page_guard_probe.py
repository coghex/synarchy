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

Usage: python3 tools/remote_warning_page_guard_probe.py [--port 9421]
"""
from __future__ import annotations

import argparse
import glob
import sys

from probelib import boot, quit_engine, send, send_json, poll_until

SPROOT = "/tmp"
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

failures: list[str] = []


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f" — {detail}" if detail and not ok else ""))
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
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9421)
    args = ap.parse_args()
    port = args.port

    proc = boot(port, f"{SPROOT}/remote_warning_page_guard_probe_engine.log")
    try:
        load_defs(port)
        init_arena(port, PAGE_A)
        init_arena(port, PAGE_B)
        init_arena(port, PAGE_C)
        check("page A becomes active", show_and_wait(port, PAGE_A))

        valid = send_json(port, f"local v = building.canPlaceAt('{PORTAL}', 0, 0); return v")
        remote = send_json(port, f"local r = building.remoteCheck('{PORTAL}', 0, 0); return r")
        check("(0,0) on a fresh arena is a valid, remote starting position",
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
        check("open() presents the modal (isOpen() true)", opened is True)

        presented = drain(port)
        check("open() records a 'presented' outcome",
              len(presented) == 1 and presented[0].get("outcome") == "presented",
              str(presented))

        # -- 2. switch the active world to page B while it's open --
        check("page B becomes active while the modal is open",
              show_and_wait(port, PAGE_B))

        closed = send_json(
            port,
            "local rw = require('scripts.build_tool_remote_warning'); "
            "rw.establishHere(); return rw.isOpen()")
        check("establishHere() closes the modal despite the page switch",
              closed is False)

        rejected_outcomes = drain(port)
        confirmed = [o for o in rejected_outcomes if o.get("outcome") == "confirmed"]
        rejected = [o for o in rejected_outcomes
                    if o.get("outcome") == "revalidationRejected"]
        accepted = [o for o in rejected_outcomes
                    if o.get("kind") == "buildTool.commitPlacement"]
        check("establishHere() records exactly one revalidationRejected",
              len(rejected) == 1, str(rejected_outcomes))
        check("the rejection reason distinguishes an active-world change",
              bool(rejected) and rejected[0].get("reason") == "active world changed",
              str(rejected_outcomes))
        check("a 'confirmed' outcome is still recorded (matches the ordinary "
              "revalidation-failure contract)",
              len(confirmed) == 1, str(rejected_outcomes))
        check("no buildTool.commitPlacement outcome was recorded",
              len(accepted) == 0, str(rejected_outcomes))

        listing_after_reject = send(port, "return building.list()")
        check("no portal was spawned on either page",
              PORTAL not in listing_after_reject, listing_after_reject)

        # -- 3. sanity: the happy path (same page throughout) is unaffected --
        check("page C becomes active", show_and_wait(port, PAGE_C))
        drain(port)  # clear noise

        send(port,
             f"local rw = require('scripts.build_tool_remote_warning'); "
             f"rw.open('{PORTAL}', 4, 4, nil, 128); return rw.isOpen()")
        happy_closed = send_json(
            port,
            "local rw = require('scripts.build_tool_remote_warning'); "
            "rw.establishHere(); return rw.isOpen()")
        check("same-page establishHere() closes the modal", happy_closed is False)

        happy_outcomes = drain(port)
        happy_accepted = [o for o in happy_outcomes
                          if o.get("kind") == "buildTool.commitPlacement"
                          and o.get("outcome") == "accepted"]
        check("same-page establishHere() still spawns the portal (accepted)",
              len(happy_accepted) == 1, str(happy_outcomes))

        listing_after_accept = send(port, "return building.list()")
        check("exactly one portal now exists (the same-page happy path)",
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
        check("chooseAnotherSite() still cancels normally", cancel_closed is False)
        cancel_outcomes = drain(port)
        check("chooseAnotherSite() records 'canceled', spawning nothing",
              len(cancel_outcomes) == 1
              and cancel_outcomes[0].get("outcome") == "canceled",
              str(cancel_outcomes))
        listing_after_cancel = send(port, "return building.list()")
        check("cancel path still spawned nothing new (still exactly one portal)",
              listing_after_cancel.count(PORTAL) == 1, listing_after_cancel)

    finally:
        quit_engine(port, proc)

    if failures:
        print(f"\n{len(failures)} check(s) FAILED:")
        for f in failures:
            print(f"  - {f}")
        return 1
    print("\nAll checks passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
