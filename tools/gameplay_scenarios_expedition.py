#!/usr/bin/env python3
"""The ``expedition`` scenario of the manual gameplay runner (#925, #2151).

Five acolytes + one technomule (the real starting party) on a
repeatable fixed-seed world: seed 42, size 64, three plates. Worldgen
output is bit-identical across platforms, so the base camp and the
fixed out-and-back route derived from that terrain are the same on
every run without shipping a world asset. Two acolytes are provisioned
off the STATIONARY mule through the capacity-gated transfer (#1212) and
walk the route under the real player move order; body/inventory/injury
checkpoints are recorded at every waypoint and anything that ended a
trip early is an OBSERVATION, never a failure.

This module owns the fixed-world constants, the deterministic camp
search and its terrain-band scan, the route, the expedition report and
``run_expedition``. It consumes ``gameplay_scenarios_support`` only —
never the first-aid owner and never the façade — boots exactly one
engine and shuts it down through ``quit_engine`` in a ``finally``.
Selected and dispatched by ``python3 tools/gameplay_scenarios.py --test
expedition``; not a probe, not a CI gate, and the exit status is never a
gameplay verdict (see the façade's docstring).
"""
from __future__ import annotations

import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from probelib import boot, quit_engine, send, send_json  # noqa: E402
from gameplay_scenarios_support import (  # noqa: E402
    LOG, ScenarioError, _num, bootstrap, checkpoint, print_checkpoint,
    provisioning_load, spawn_roster, transfer, walk_leg)

# --- expedition world -------------------------------------------------
# A deterministic world the runner generates itself; worldgen output is
# bit-identical across platforms, so the same seed/size/plates always
# yields the same terrain, hence the same derived camp and route.
EXP_PAGE, EXP_SEED, EXP_SIZE, EXP_PLATES = "expedition", 42, 64, 3
EXP_CHUNK_REGION = 4          # chunks each way around the origin
EXP_SCAN_HALF_WIDTH = 56      # tiles each way the camp search looks
EXP_ROWS = (0, 16, -16, 32, -32)   # candidate camp rows, in search order
EXP_LEG = 8                   # tiles per outbound leg
EXP_LEGS_OUT = 3              # 3 legs out (24 tiles), then straight back


def _scan_band(port: int, gy: int) -> list:
    """Fluid + terrain height for one east-west band (rows gy-1..gy+1)
    across the search window, in a single console round trip."""
    lua = (
        "local r = {}; "
        f"for gx = {-EXP_SCAN_HALF_WIDTH}, {EXP_SCAN_HALF_WIDTH} do "
        "local wet, lo, hi = false, nil, nil; "
        f"for dy = -1, 1 do local f = world.getFluidAt(gx, {gy} + dy); "
        "if f ~= nil then wet = true end; "
        f"local _s, t = world.getTerrainAt(gx, {gy} + dy); "
        "if t == nil then wet = true else "
        "if lo == nil or t < lo then lo = t end; "
        "if hi == nil or t > hi then hi = t end end end; "
        "r[#r+1] = {gx = gx, wet = wet, lo = lo, hi = hi} end; return r"
    )
    cells = send_json(port, lua, timeout=60.0)
    if not isinstance(cells, list):
        raise ScenarioError(f"terrain band scan failed: {cells!r}")
    return cells


def find_camp(port: int) -> tuple:
    """Pick the base camp deterministically from the (deterministic)
    world: the westmost tile of the first band run that is dry, fully
    loaded and gently graded for the whole fixed route. Same world in,
    same camp and route out — that is what makes the scenario
    repeatable without shipping a world asset."""
    need = EXP_LEG * EXP_LEGS_OUT + 2   # route length + camp + a tile of slack
    for gy in EXP_ROWS:
        cells = _scan_band(port, gy)
        run_start, prev_hi, run = None, None, 0
        for cell in cells:
            gx = int(cell["gx"])
            lo, hi = _num(cell.get("lo")), _num(cell.get("hi"))
            # Usable: the whole 3-row band here is dry, loaded and flat
            # across the band. Steppable: also within one z of the tile
            # before it — a cheap stand-in for "walkable, no cliffs".
            usable = (not cell.get("wet") and lo is not None
                      and hi is not None and abs(hi - lo) <= 1.0)
            steppable = usable and (prev_hi is None or abs(hi - prev_hi) <= 1.0)
            if steppable:
                if run == 0:
                    run_start = gx
                run += 1
            elif usable:
                run, run_start = 1, gx     # a fresh run starts here
            else:
                run, run_start = 0, None
            prev_hi = hi if usable else None
            if run >= need:
                return run_start, gy
    raise ScenarioError(
        "no dry, gently-graded stretch long enough for the fixed route was "
        f"found on seed {EXP_SEED}/size {EXP_SIZE}/plates {EXP_PLATES} "
        f"(searched rows {EXP_ROWS})")


def run_expedition(port: int) -> int:
    proc = boot(port, log=LOG, label="expedition engine")
    t0 = time.time()
    observations: list[str] = []
    checkpoints: list[dict] = []
    try:
        bootstrap(port)
        print(f"generating the test world (seed {EXP_SEED}, size {EXP_SIZE}, "
              f"plates {EXP_PLATES}) ...")
        send(port, f"world.init('{EXP_PAGE}', {EXP_SEED}, {EXP_SIZE}, "
                   f"{EXP_PLATES})", expect_result=False)
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, f"world.show('{EXP_PAGE}')", expect_result=False)
        r = EXP_CHUNK_REGION
        send(port, f"world.loadChunksInRegion({-r}, {-r}, {r}, {r}); "
                   f"return 'ok'", timeout=60.0)
        send(port, "return world.waitForChunks(300)", timeout=310)

        camp_x, camp_y = find_camp(port)
        print(f"base camp: ({camp_x}, {camp_y})")
        camp_tiles = [(camp_x, camp_y), (camp_x, camp_y + 1),
                      (camp_x, camp_y - 1), (camp_x + 1, camp_y + 1),
                      (camp_x + 1, camp_y - 1),      # 5 acolytes
                      (camp_x + 1, camp_y)]          # technomule
        uids = spawn_roster(port, EXP_PAGE, camp_tiles)
        acolytes, mule = uids[:5], uids[5]
        party = acolytes[:2]     # the two acolytes chosen for the trip

        # Provision the party off the STATIONARY mule through the real
        # inventory-transfer path. The mule stays at camp as the supply
        # point; it never travels. Every move is gated on the prospective
        # instance's own weight (#1212), so no traveller starts the route
        # over its carrying capacity and the encumbrance the route
        # measures is the one the provisioning intended.
        moved = []
        for uid in party:
            moved.append((uid, "rations", transfer(port, mule, uid,
                                                   "rations", 3)))
        moved.append((party[0], "first_aid_kit",
                      transfer(port, mule, party[0], "first_aid_kit", 1)))
        for uid, name, res in moved:
            # A partial move is reported too: moving one of three rations
            # still means an item was turned away, and which one it was
            # is exactly what a provisioning observation is for.
            if res.refused:
                observations.append(
                    f"provisioning acolyte {uid} off the mule ({mule}) "
                    f"stopped after {res.moved} of the requested {name}: "
                    f"{res.detail}")
        loads = [(uid, provisioning_load(port, uid)) for uid in party]

        checkpoints.append(checkpoint(port, "prepared at camp",
                                      party + [mule], t0))

        waypoints = [(camp_x + EXP_LEG * i, camp_y,
                      f"outbound leg {i}/{EXP_LEGS_OUT}")
                     for i in range(1, EXP_LEGS_OUT + 1)]
        waypoints.append((camp_x, camp_y, "return to camp"))
        for tx, ty, label in waypoints:
            print(f"  walking: {label} -> ({tx}, {ty})")
            legs = EXP_LEGS_OUT if label.startswith("return") else 1
            walk_leg(port, party, tx, ty, budget=15.0 * EXP_LEG * legs,
                     observations=observations, leg=label)
            checkpoints.append(checkpoint(port, label, party, t0))

        checkpoints.append(checkpoint(port, "final state",
                                      party + [mule], t0))

        print("\n" + "=" * 72)
        print("EXPEDITION SCENARIO REPORT")
        print("=" * 72)
        print(f"world          seed {EXP_SEED}, size {EXP_SIZE}, "
              f"plates {EXP_PLATES} (deterministic, generated by this run)")
        print(f"base camp      ({camp_x}, {camp_y}) — derived deterministically "
              f"from that world")
        print(f"route          {EXP_LEGS_OUT} legs of {EXP_LEG} tiles east, "
              f"then straight back ({2 * EXP_LEG * EXP_LEGS_OUT} tiles total)")
        print(f"roster         {len(acolytes)} acolytes + 1 technomule "
              f"(uids {uids}), player faction")
        print(f"party          acolytes {party[0]} and {party[1]}")
        print(f"supply point   technomule {mule}, stationary at camp")
        print("provisioning   " + ", ".join(
            f"{res.moved}x {name} -> {uid}" for uid, name, res in moved))
        print("               every move was gated on the chosen instance's "
              "own weight, so no\n               traveller can finish "
              "provisioning above its carrying capacity:")
        for uid, load in loads:
            print(f"               acolyte {uid} loaded to {load}")
        print("condition      each acolyte's standing find_water goal was "
              "retired at spawn so the fixed route is the standing order")
        for cp in checkpoints:
            print_checkpoint(cp)
        print("\n  -- observations --")
        if observations:
            for line in observations:
                print(f"    * {line}")
        else:
            print("    * the party walked the whole route and returned")
        print("\n  NOTE: this report is an observation, not a verdict. The "
              "exit status\n  reflects setup/runtime failure only — "
              "survival-pressure tuning is #919.")
        return 0
    finally:
        quit_engine(port, proc)
