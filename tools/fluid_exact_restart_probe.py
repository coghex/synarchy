#!/usr/bin/env python3
"""Fresh-process gate for one PARTIAL fluid cell's exact level (#2535,
DFL-6 of epic #2514).

Every earlier slice proved its own link: #2520 that the exact surface
survives activation, writeback, deactivation and the save codec; #2529
that a partial level renders; #2533 that generated rivers carry one. None
proved the whole chain in one run, from a player edit to a fresh engine,
and every console reader before this slice saw only the integer ceiling,
so a partial cell silently rounding to a full one read as a pass.

This drives that chain once and grades it on the EXACT state, read
through ``world.getAreaFluid``'s additive ``surfaceUnits`` / ``level``
fields:

  1. Engine A: boot headless against an isolated resource root and
     ``world.init`` a small REAL GENERATED page (never an arena --
     ``docs/headless_console.md`` NB #365: a save holding one hangs the
     load). Find a dry, level land tile with dry, level orthogonal
     neighbours.
  2. Place ONE whole level of lake there with ``world.setFluidTile``.
     That edit writes a FULL cell (level 8) and nothing else, so the
     edited cell showing a level of 1..7 is proof the simulation flowed
     it and a writeback published the result. Wait for exactly that.
  3. Pause, then require the neighbourhood's exact state to hold still
     across repeated reads before recording it -- a writeback already in
     flight when the pause landed must not move a cell after the
     expectation was captured.
  4. Save, and wait for THAT request to reach ``SaveCaptureComplete``.
     Quit.
  5. Engine B: a fresh process on the same root. Load, wait for THAT
     request to publish, require the session paused, and compare every
     recorded cell -- type, exact units, level, integer ceiling and
     terrain -- with nothing missing and nothing extra, twice, a second
     apart.

Each recorded cell must also satisfy the two compatibility identities
the diagnostics promise: ``surface`` (``fluidSurf`` in the dump) is the
mathematical ceiling of ``surfaceUnits / 8``, and ``level`` is
``1 + ((surfaceUnits - 1) mod 8)``.

Registered manual-only in ``tools/ci_probes.py`` (#2809: a live simulation,
optional local evidence, never CI). Its GPU counterpart,
``tools/fluid_exact_restart_render_probe.py``, reuses steps 1-4 and grades
the reloaded cell's PIXELS; it is manual-only.

Usage:
  python3 tools/fluid_exact_restart_probe.py [--port 9285]
"""
from __future__ import annotations

import argparse
import json
import os
import sys
import tempfile
import time
from pathlib import Path

from probelib import (boot, quit_engine, send, send_json, capture_request_id,
                      wait_load_published, wait_save_complete)

REPO = Path(__file__).resolve().parent.parent

SEED = 771
WORLD_SIZE = 8
PLATE_COUNT = 3
PAGE = "fluid_exact_restart_page"
SLOT = "fluid_exact_restart_probe"
# The neighbourhood recorded and compared around the edited cell. One
# whole level of lake spread over level ground stays well inside it.
RADIUS = 3
# How long the flow has to turn the edited cell partial: the edit, the
# sim's activation, a few 10 Hz ticks, and the writeback.
FLOW_TIMEOUT = 60.0
# How long a paused neighbourhood has to stop changing, and how many
# consecutive identical reads count as "stopped".
SETTLE_TIMEOUT = 20.0
STABLE_READS = 3
STABLE_INTERVAL = 0.5


class Checks:
    def __init__(self) -> None:
        self.failed = 0

    def ok(self, cond: bool, label: str) -> bool:
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}")
        if not cond:
            self.failed += 1
        return cond


def make_isolated_root(base: str) -> str:
    """A throwaway resource root: real read-only content symlinked in,
    plus its OWN empty saves/ -- the pattern
    ``tools/fluid_reaction_probe.py`` and
    ``tools/persistence_contract_probe.py`` use, so this probe never
    touches the developer's real saves."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def as_int(value):
    try:
        return int(float(value))
    except (TypeError, ValueError):
        return None


def ceil_div8(units: int) -> int:
    return -(-units // 8)


def level_of(units: int) -> int:
    return 1 + ((units - 1) % 8)


def area_fluid(port: int, cx: int, cy: int, radius: int = RADIUS):
    """Every fluid cell ``world.getAreaFluid`` reports around (cx, cy),
    keyed by coordinate, or None when the reply is not a list.

    The verb scans the ACTIVE page, so callers check the active page
    first; each entry's integer fields are normalized to ``int`` so two
    reads compare by value."""
    raw = send_json(port, f"return world.getAreaFluid({cx}, {cy}, {radius})")
    if raw == {}:
        return {}  # an empty Lua table encodes as an object
    if not isinstance(raw, list):
        return None
    cells = {}
    for entry in raw:
        if not isinstance(entry, dict):
            return None
        x, y = as_int(entry.get("x")), as_int(entry.get("y"))
        cells[(x, y)] = {
            "type": entry.get("type"),
            "surface": as_int(entry.get("surface")),
            "surfaceUnits": as_int(entry.get("surfaceUnits")),
            "level": as_int(entry.get("level")),
            "terrainZ": as_int(entry.get("terrainZ")),
        }
    return cells


def active_id(port: int) -> str:
    return send(port, "return world.getActiveWorldId()").strip().strip('"')


def terrain_at(port: int, gx: int, gy: int):
    return as_int(send(
        port, f"local s, t = world.getTerrainAt({gx}, {gy}, '{PAGE}'); return t"))


def find_level_site(port: int):
    """A dry land tile above sea level whose four orthogonal neighbours
    are dry and at the SAME terrain height.

    Level ground is what makes the flow split the placed level rather
    than drain it: the lateral phase moves fluid across equal terrain,
    so a single level placed there ends as several partial cells. One
    Lua pass does the whole scan, so the search is one round trip."""
    # The console evaluates ONE line per request, so the chunk is joined
    # onto one; Lua does not care.
    lua = f"""
local page = '{PAGE}'
local function dry(x, y) return world.getFluidAt(x, y) == nil end
local function top(x, y) local s, t = world.getTerrainAt(x, y, page); return s, t end
for r = 0, 12 do
  for gy = -r, r do
    for gx = -r, r do
      if math.max(math.abs(gx), math.abs(gy)) == r then
        local s, t = top(gx, gy)
        if t and t > 0 and s == t and dry(gx, gy) then
          local good = true
          for _, d in ipairs({{{{1,0}},{{-1,0}},{{0,1}},{{0,-1}}}}) do
            local ns, nt = top(gx + d[1], gy + d[2])
            if nt ~= t or ns ~= nt or not dry(gx + d[1], gy + d[2]) then
              good = false
            end
          end
          if good then return {{x = gx, y = gy, t = t}} end
        end
      end
    end
  end
end
return nil"""
    raw = send_json(port, " ".join(lua.split("\n")), timeout=30)
    if not isinstance(raw, dict):
        return None, None
    return (as_int(raw.get("x")), as_int(raw.get("y"))), as_int(raw.get("t"))


def cell_identity_problems(cells) -> list[str]:
    """Every recorded cell's violations of the two compatibility
    identities: the integer surface is the ceiling of the exact plane,
    and the level is the exact plane's top fill in 1..8."""
    problems = []
    for coord, c in sorted(cells.items()):
        u = c["surfaceUnits"]
        if u is None or c["level"] is None or c["surface"] is None:
            problems.append(f"{coord}: missing exact fields {c}")
            continue
        if c["surface"] != ceil_div8(u):
            problems.append(f"{coord}: surface {c['surface']} is not "
                            f"ceil({u}/8) = {ceil_div8(u)}")
        if c["level"] != level_of(u):
            problems.append(f"{coord}: level {c['level']} is not "
                            f"1 + (({u} - 1) mod 8) = {level_of(u)}")
    return problems


def wait_partial(port: int, tile, seconds: float):
    """Poll until the edited cell is fluid at a PARTIAL level (1..7).
    Answers the last observed cell (None if it never appeared)."""
    deadline = time.time() + seconds
    last = None
    while time.time() < deadline:
        cells = area_fluid(port, tile[0], tile[1], 0) or {}
        last = cells.get(tuple(tile))
        if last is not None and last["level"] is not None \
                and 1 <= last["level"] <= 7:
            return last
        time.sleep(0.25)
    return last


def stable_area(port: int, tile, seconds: float):
    """The neighbourhood once ``STABLE_READS`` consecutive reads agree,
    or None if it never holds still within ``seconds``."""
    deadline = time.time() + seconds
    streak = []
    while time.time() < deadline:
        cells = area_fluid(port, *tile)
        if cells is None:
            streak = []
        elif streak and cells != streak[-1]:
            streak = [cells]
        else:
            streak.append(cells)
        if len(streak) >= STABLE_READS:
            return streak[-1]
        time.sleep(STABLE_INTERVAL)
    return None


def build_partial_save(chk: Checks, port: int):
    """Engine A's whole scenario, steps 1-4. Answers the scenario record
    ``{"tile", "cell", "area"}`` once a save holding a partial cell has
    completed, or None."""
    send(port,
         f"world.init('{PAGE}', {SEED}, {WORLD_SIZE}, {PLATE_COUNT}, "
         f"'Fluid Exact Restart Probe', 'the exact fluid restart probe world'); "
         f"return 'ok'")
    send(port, f"world.show('{PAGE}'); return 'ok'")
    send(port, "return world.waitForInit(120)", timeout=130)
    active = active_id(port)
    if not chk.ok(active == PAGE, f"'{PAGE}' is the active world (got {active!r})"):
        return None

    tile, terrain = find_level_site(port)
    if not chk.ok(tile is not None,
                  f"found a dry level land site with dry level neighbours "
                  f"(tile {tile}, terrain top {terrain})"):
        return None

    send(port, f"world.setFluidTile('{PAGE}', {tile[0]}, {tile[1]}, 'water'); "
               f"return 'ok'")
    cell = wait_partial(port, tile, FLOW_TIMEOUT)
    if not chk.ok(cell is not None and 1 <= (cell["level"] or 0) <= 7,
                  f"the flow turned the edited full cell {tile} PARTIAL -- "
                  f"only a simulation writeback can (got {cell})"):
        return None

    send(port, "engine.setPaused(true); return 'ok'")
    paused = send(port, "return engine.isPaused()").strip() == "true"
    if not chk.ok(paused, "the session is paused before the expectation is captured"):
        return None
    area = stable_area(port, tile, SETTLE_TIMEOUT)
    if not chk.ok(area is not None,
                  f"the paused neighbourhood held still for {STABLE_READS} "
                  f"reads {STABLE_INTERVAL}s apart"):
        return None
    cell = area.get(tuple(tile))
    if not chk.ok(cell is not None and 1 <= (cell["level"] or 0) <= 7,
                  f"the edited cell is still partial once settled (got {cell})"):
        return None
    problems = cell_identity_problems(area)
    chk.ok(not problems,
           f"every recorded cell's integer surface is its exact ceiling and "
           f"its level its exact top fill ({len(area)} cells; {problems[:3]})")
    partial = sorted(c for c, v in area.items() if 1 <= v["level"] <= 7)
    print(f"  [note] recorded {len(area)} cells, {len(partial)} partial; "
          f"edited cell {tile}: {cell}")

    saved = send(port, f"return engine.saveWorld('{PAGE}', '{SLOT}')")
    if not chk.ok(saved.strip() == "true",
                  f"engine.saveWorld('{SLOT}') accepted (got {saved!r})"):
        return None
    request_id = capture_request_id(port, "return engine.getSaveStatus()")
    if not chk.ok(request_id is not None,
                  "engine.getSaveStatus() reports the save's request id"):
        return None
    ok, status = wait_save_complete(port, request_id)
    if not chk.ok(ok, f"the save reached SaveCaptureComplete (got {status!r})"):
        return None
    after = area_fluid(port, *tile)
    chk.ok(after == area,
           "the paused neighbourhood did not move while the save ran")
    return {"tile": list(tile), "cell": cell,
            "area": {f"{x},{y}": v for (x, y), v in sorted(area.items())}}


def load_saved(chk: Checks, port: int) -> bool:
    """Engine B's load, published and shown. True once the page is the
    active world of a paused session."""
    accepted = send(port, f"return engine.loadSave('{SLOT}')")
    if not chk.ok(accepted.strip() == "true",
                  f"engine.loadSave('{SLOT}') accepted (got {accepted!r})"):
        return False
    request_id = capture_request_id(port, "return engine.getLoadStatus()")
    published, status = wait_load_published(port, request_id=request_id)
    if not chk.ok(published, f"the load reached LoadPublished (got {status!r})"):
        return False
    send(port, f"world.show('{PAGE}'); return 'ok'")
    send(port, "return world.waitForInit(120)", timeout=130)
    active = active_id(port)
    if not chk.ok(active == PAGE, f"'{PAGE}' is the reloaded active world "
                                  f"(got {active!r})"):
        return False
    paused = send(port, "return engine.isPaused()").strip() == "true"
    return chk.ok(paused, "the loaded session is paused")


def expected_area(scenario) -> dict:
    return {tuple(int(v) for v in k.split(",")): cell
            for k, cell in scenario["area"].items()}


def wait_reloaded_area(port: int, scenario, seconds: float = 60.0):
    """The reloaded neighbourhood, polled until the edited cell's chunk
    has loaded (its cell reads back at all). Answers the last read."""
    tile = scenario["tile"]
    deadline = time.time() + seconds
    cells = None
    while time.time() < deadline:
        cells = area_fluid(port, *tile)
        if cells and tuple(tile) in cells:
            return cells
        time.sleep(0.5)
    return cells


def verify_reload(chk: Checks, port: int, scenario) -> None:
    """Steps 5's comparison against the recorded scenario."""
    tile = tuple(scenario["tile"])
    expected = expected_area(scenario)
    cells = wait_reloaded_area(port, scenario)
    chk.ok(cells is not None and tile in cells,
           f"the edited cell {tile} reads back after the load")
    if not cells:
        return
    got = cells.get(tile)
    want = expected[tile]
    chk.ok(got == want,
           f"the edited cell's exact units, level, type and ceiling survived "
           f"the fresh-process load (got {got}, expected {want})")
    missing = sorted(set(expected) - set(cells))
    extra = sorted(set(cells) - set(expected))
    changed = sorted(c for c in set(expected) & set(cells)
                     if cells[c] != expected[c])
    chk.ok(not missing and not extra and not changed,
           f"every recorded neighbourhood cell survived exactly "
           f"(missing {missing}, extra {extra}, changed "
           f"{[(c, expected[c], cells[c]) for c in changed][:3]})")
    problems = cell_identity_problems(cells)
    chk.ok(not problems,
           f"every reloaded cell's surface is its exact ceiling and its level "
           f"its exact top fill ({problems[:3]})")
    time.sleep(1.0)
    chk.ok(area_fluid(port, *tile) == cells,
           "the reloaded neighbourhood stays frozen while paused")


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9285)
    ap.add_argument("--scenario-out", default=None,
                    help="also write the recorded scenario JSON here")
    args = ap.parse_args()
    port = args.port

    chk = Checks()
    with tempfile.TemporaryDirectory(prefix="fluid_exact_restart_probe_") as base:
        root = make_isolated_root(base)
        log_a = os.path.join(base, "engine_a.log")
        log_b = os.path.join(base, "engine_b.log")

        print("== engine A: generate, flow a partial cell, record, save ==")
        proc = boot(port, log=log_a, args=["--resource-root", root],
                    ready_timeout=180)
        try:
            scenario = build_partial_save(chk, port)
        finally:
            quit_engine(port, proc)
        if scenario is None:
            print("\nFAILED: no saved partial cell to reload.")
            return 1
        if args.scenario_out:
            Path(args.scenario_out).write_text(json.dumps(scenario, indent=2) + "\n")

        print("== engine B: fresh process, load, compare while paused ==")
        proc = boot(port, log=log_b, args=["--resource-root", root],
                    ready_timeout=180)
        try:
            if load_saved(chk, port):
                verify_reload(chk, port, scenario)
        finally:
            quit_engine(port, proc)

    print()
    if chk.failed:
        print(f"FAILED: {chk.failed} check(s)")
        return 1
    print("PASSED: a partial fluid cell's exact level survives a fresh-process "
          "save/load")
    return 0


if __name__ == "__main__":
    sys.exit(main())
