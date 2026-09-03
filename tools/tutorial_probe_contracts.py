#!/usr/bin/env python3
"""Public contracts, the run's single failure collector, and tutorial
progress state for `tools/tutorial_probe.py` (#2145).

The lowest layer of the probe: it imports no sibling module and owns the
things every other one reads — the page and save-slot names, the
objective and subobjective ids from data/tutorials/first_session.yaml,
the item and building def names, `ProbeError`, the `Checks` recorder
every assertion goes through, and the `Progress` snapshot the tutorial
modules' own public read API is parsed into.

`Checks` is constructed ONCE, by the facade, and handed down to every
stage. A stage that built its own recorder would report its failures
into a list nobody prints, and the run would exit 0 with checks failed
— which is why the collector is an explicitly passed object rather than
the module-level list this probe used before the split.
"""
from __future__ import annotations

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import send, poll_until

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

PAGE = "tutorial"
SLOT = "tutorial_probe_slot"
#: The pre-latched leg's own save slot, kept apart from SLOT so the two
#: round trips can never observe each other's generation.
STICKY_SLOT = "tutorial_probe_sticky_slot"

PORTAL_DEF = "acolyte_portal"
CANTEEN_DEF = "canteen_steel_2l"
RATIONS_DEF = "rations"
# scripts/tutorial_eval.lua's EXPEDITION_WATER_L — one full canteen.
EXPEDITION_WATER_L = 2.0

# Objective ids, from data/tutorials/first_session.yaml.
OBJ_PORTAL = "first_session_place_portal"
OBJ_WATER = "first_session_secure_water"
OBJ_EXPEDITION = "first_session_prepare_expedition"
SUB_WATER = "first_session_prepare_water"
SUB_FOOD = "first_session_prepare_food"


class ProbeError(RuntimeError):
    """Setup failure — the probe could not reach the state it tests."""


class Checks:
    """The run's ONE failure collector, shared by every module.

    Both stage owners and the facade record into the same instance: the
    facade's `FAILED (n):` report and its exit status are computed from
    `failures`, so a stage that recorded anywhere else would pass a run
    that failed. The facade constructs exactly one and hands it down.
    """

    def __init__(self) -> None:
        self.failures: list[str] = []

    def check(self, name: str, ok: bool, detail: str = "") -> bool:
        print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
              + (f" — {detail}" if detail and not ok else ""))
        if not ok:
            self.failures.append(name if not detail else f"{name} — {detail}")
        return ok

    def fail(self, message: str) -> None:
        """Record an operational failure — a leg that raised, or a leg
        whose precondition never existed — with no PASS/FAIL line of its
        own, exactly as the facade's leg handlers always have."""
        self.failures.append(message)


# --------------------------------------------------------------------------
# Tutorial state, read through the module's own public surface
# --------------------------------------------------------------------------
class Progress:
    """One snapshot of tutorial progress + the panel's view model.

    Completion and check state come from the module's own read API
    (`completedIds` / `isSubobjectiveChecked`), NOT from the view model:
    the view model is reveal-filtered, and a composite that is completed
    with every subobjective checked HIDES, which drops its subobjective
    rows out of the model entirely. Reading a check off a row would then
    report "unchecked" for the very transition that hid it.

    Read as a delimited STRING rather than a JSON table: the view model
    leaves `completed` unset on subobjective rows and `checked` unset on
    full rows, and an empty set is an empty Lua table — both of which
    serialize ambiguously. Spelling every field with tostring() keeps
    "absent" and "false" distinguishable at the Python end.
    """

    def __init__(self, raw: str) -> None:
        parts = raw.split(";")
        head = parts[0] if parts else ""
        completed, _, checked = head.partition("#")
        self.completed = [c for c in completed.split(",") if c]
        self.checked = [c for c in checked.split(",") if c]
        self.rows: list[dict] = []
        for chunk in parts[1:]:
            if not chunk:
                continue
            rid, kind, active, completed, checked = chunk.split(":")
            self.rows.append({
                "id": rid, "kind": kind,
                "active": active == "true",
                "completed": None if completed == "nil" else completed == "true",
                "checked": None if checked == "nil" else checked == "true",
            })

    @property
    def row_ids(self) -> list[str]:
        return [r["id"] for r in self.rows]

    @property
    def active_row_ids(self) -> list[str]:
        """Only the rows in the default checklist view -- `row_ids`
        alone also retains completed history (#958's `active = false`
        rows), which is the right thing for "was this ever revealed" but
        the wrong thing for "what does the checklist show right now"."""
        return [r["id"] for r in self.rows if r["active"]]

    def row(self, rid: str) -> dict | None:
        for r in self.rows:
            if r["id"] == rid:
                return r
        return None

    def is_completed(self, rid: str) -> bool:
        return rid in self.completed

    def is_checked(self, rid: str) -> bool:
        return rid in self.checked

    def __str__(self) -> str:
        return (f"completed={self.completed} checked={self.checked} rows="
                + " ".join(f"{r['id']}(c={r['completed']},k={r['checked']},"
                           f"a={r['active']})" for r in self.rows))


PROGRESS_LUA = (
    "local tp = require('scripts.tutorial_progress'); "
    "local m = tp.getViewModel(); "
    "local ck = {}; "
    "for _, id in ipairs(tp.index and tp.index.order or {}) do "
    "if tp.isSubobjectiveChecked(id) then ck[#ck+1] = id end end; "
    "local out = { table.concat(tp.completedIds(), ',') .. '#' "
    ".. table.concat(ck, ',') }; "
    "for _, r in ipairs(m.rows) do out[#out+1] = r.id .. ':' .. r.kind "
    ".. ':' .. tostring(r.active) .. ':' .. tostring(r.completed) "
    ".. ':' .. tostring(r.checked) end; "
    "return table.concat(out, ';')"
)


def progress(port: int) -> Progress:
    return Progress(send(port, PROGRESS_LUA, timeout=15.0))


def settle(port: int, pred, seconds: float = 15.0) -> Progress:
    """Wait for the evaluation tick to publish a state satisfying `pred`.

    The evaluator recomputes from live state every tick, so a fact the
    probe just changed becomes visible within one tick — but "one tick"
    is not synchronous with the console command that changed it. Polling
    the predicate (rather than sleeping) keeps the probe honest: a check
    that never becomes true fails on its own assertion below, with the
    LAST observed state reported.
    """
    def once() -> Progress | None:
        p = progress(port)
        return p if pred(p) else None

    got = poll_until(seconds, once)
    return got if got is not None else progress(port)


def hud_open(port: int) -> str:
    """Whether the tutorial panel is expanded.

    Shared support rather than a stage owner's helper: both round trips
    assert that a load brings the HUD back COLLAPSED, so this is the one
    tutorial_hud reader genuinely used by both stage owners. The rest of
    the HUD surface (`show_gameplay_hud`, `open_checklist`,
    `checklist_rows`, `hud_visible`) is the sticky-presentation owner's
    alone and lives there.
    """
    return send(port,
                "local h = package.loaded['scripts.tutorial_hud']; "
                "if not h then return 'absent' end; "
                "return tostring(h.dump().open)", timeout=15.0)
