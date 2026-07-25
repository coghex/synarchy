#!/usr/bin/env python3
"""Manual first-expedition gameplay scenarios (#925).

A deliberately small, ON-DEMAND scenario runner for watching real
gameplay behavior in the first-expedition arc. Two scenarios, both
one-shot:

  expedition  Five acolytes + one technomule (the real starting party)
              on a repeatable fixed-seed world. Two acolytes are
              provisioned off the STATIONARY mule and walk a fixed
              out-and-back route; body/inventory/injury checkpoints are
              recorded at every waypoint.
  first-aid   The same real starting roster on a repeatable arena. The
              mule's pre-stocked first-aid kit is moved onto the selected
              expedition acolyte, who then takes a real fall; the injury,
              the treatment attempt/outcome, the kit state and the final
              unit state are reported.

THIS IS NOT A BEHAVIOR PROBE AND NOT A CI GATE. It is deliberately
absent from ``tools/run_probes.py`` and ``tools/ci_probes.py``, is never
selected by CI, and is not named ``*_probe.py`` — the probe registry and
its classification self-test key off registered probe names only.

**Exit status means setup/runtime failure only, never a gameplay
verdict.** 0 = the scenario finished its setup and printed its report;
1 = the engine/setup/console broke before the report could be produced;
2 = a bad command line. A unit dying, starving, failing to reach a
waypoint or going untreated is a reported OBSERVATION, not a failure —
survival-pressure tuning is #919's job, not this script's.

Usage:
  python3 tools/gameplay_scenarios.py --list
  python3 tools/gameplay_scenarios.py --test expedition
  python3 tools/gameplay_scenarios.py --test first-aid
  python3 tools/gameplay_scenarios.py --test expedition --port 9926

Engine hygiene (same conventions as tools/*_probe.py, via probelib):
the default port is 9925 — never 8008, the user's GUI — ``--port``
overrides it, and every scenario shuts its own engine down through
``engine.quit()`` (hard-killing its own tracked PID as a fallback) in a
``finally``. Nothing is saved, so the repository's runtime state is
untouched apart from the engine's normal boot-time materialization of
gitignored ``config/*.local.yaml``.
"""
from __future__ import annotations

import argparse
import glob
import os
import sys
import time

from probelib import boot, quit_engine, send, send_json, clear_find_water

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DEFAULT_PORT = 9925
LOG = "/tmp/gameplay_scenarios_engine.log"

# The real starting party (scripts/building_spawn.lua's acolyte_portal
# roster): five acolytes, then the technomule that hauls the stock.
ROSTER = ["acolyte"] * 5 + ["technomule"]
PLAYER_FACTION = "player"

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
ARRIVAL_TILES = 0.8           # matches the AI's own 0.6 + a little slack

# --- first-aid arena --------------------------------------------------
# A ridge the scout walks off. It spans far more than the local A*
# search radius (16 tiles) in y, so there is no way around it inside the
# planner's horizon and the only route east is over the edge.
#
# The height is the SHALLOWEST real fall: `pcFallTriggerDrop` is 2, so a
# 2-z step is the first descent the mover turns into a Falling
# transition at all. Measured on a baseline acolyte, that already lands
# ~30 wounds (blunt + fractures) and a live bleed — a genuine patient —
# while every taller drop tested (3/4/5 z) drained the unit's blood
# far enough that it was dying or dead before a medic could reach it,
# which leaves nothing to treat. Whether it survives is still NOT
# asserted; that balance question belongs to #919.
FA_RIDGE_X1, FA_RIDGE_X2 = -4, 0
FA_RIDGE_Y1, FA_RIDGE_Y2 = -20, 20
FA_RIDGE_H = 2
FA_LOAM = 56                  # movement_arena's loam material id
FA_SCOUT_TILE = (-2, 0)       # on top of the ridge
FA_MEDIC_TILE = (2, 0)        # waiting at the foot, beside the landing
FA_CAMP_X = 4                 # low ground east of the ridge
FA_LANDING = (1, 0)           # the tile the fall lands on

YAML_LOADERS = [
    ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
    ("data/infections/*.yaml", "engine.loadInfectionYaml"),
    ("data/items/*.yaml",      "engine.loadItemYaml"),
    ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
    ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
    ("data/flora/*.yaml",      "engine.loadFloraYaml"),
    ("data/units/*.yaml",      "engine.loadUnitYaml"),
]

# Reported per checkpoint. `hunger` is the stomach meter and `calories`
# the energy store it feeds (see scripts/unit_resource_config.lua).
BODY_STATS = [
    ("hunger", "max_hunger"),
    ("calories", "max_calories"),
    ("hydration", "max_hydration"),
    ("exhaustion", "max_exhaustion"),
    ("stamina", "max_stamina"),
]


class ScenarioError(RuntimeError):
    """Setup/runtime failure — never a gameplay outcome."""


# ---------------------------------------------------------------------
# Engine bootstrap
# ---------------------------------------------------------------------
def bootstrap(port: int) -> None:
    """Load the gameplay catalogs + the stat/resource/AI script stack the
    loading screen would load in the GUI. unit_ai stays ACTIVE — its
    behavior is the thing these scenarios exist to watch."""
    for pattern, fn in YAML_LOADERS:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    for script, dt in [("unit_stats", 0.1), ("unit_resources", 0.2),
                       ("unit_ai", 0.1)]:
        send(port, f"engine.loadScript('scripts/{script}.lua', {dt}); "
                   f"return 'ok'")
    _install_snapshot_fn(port)


def _install_snapshot_fn(port: int) -> None:
    """Define the per-unit checkpoint reader once, so a checkpoint is one
    console round trip per unit instead of a dozen."""
    lua = (
        "function _SCEN_snap(uid) "
        "local i = unit.getInfo(uid); "
        "if not i then return {gone=true} end; "
        "local t = {gone=false, defName=i.defName, name=i.name, "
        "displayName=i.displayName, gridX=i.gridX, gridY=i.gridY, "
        "gridZ=i.gridZ, anim=i.currentAnim, moveSpeed=i.moveSpeed, "
        "knockedDown=i.knockedDown}; "
        "t.pose = unit.getPose(uid); "
        "t.carrying = unit.getCarryingWeight(uid); "
        "t.capacity = unit.getStat(uid, 'carrying_capacity'); "
        "t.blood = unit.getBlood(uid); "
        "t.pain = unit.getPain(uid); "
        "t.stats = unit.getAllStats(uid); "
        "t.wounds = unit.getWounds(uid); "
        "t.inventory = unit.getInventory(uid); "
        "local ai = package.loaded['scripts.unit_ai']; "
        "if ai and ai.getState then local s = ai.getState(uid); if s then "
        "t.action = s.currentAction; t.role = s.role; "
        "t.commanded = (s.commandedTask ~= nil); "
        "t.treating = s.treatClaim and s.treatClaim.patient end end; "
        "return t end; return 'ok'"
    )
    if send(port, lua) != "ok":
        raise ScenarioError("could not install the checkpoint reader")


def spawn_roster(port: int, page: str, tiles) -> list[int]:
    """Spawn the real starting party at `tiles` ([(x, y), ...], one per
    ROSTER entry) as PLAYER-faction units, the same way the acolyte
    portal spawns them (scripts/building_spawn.lua) — engine-side spawn
    applies each def's real starting_inventory, including the mule's
    pre-stocked first-aid kit.

    A fresh acolyte's standing `find_water` goal is retired: its search
    utility out-competes a commanded route and can walk a scout off a
    cliff, so every probe in tools/ quiets it. That is a deliberate
    scenario condition, and the report says so."""
    if len(tiles) != len(ROSTER):
        raise ScenarioError("roster/tile count mismatch")
    uids: list[int] = []
    for defname, (x, y) in zip(ROSTER, tiles):
        raw = send(port, f"return unit.spawn('{defname}', {x}, {y}, nil, "
                         f"'{PLAYER_FACTION}', '{page}')")
        try:
            uid = int(float(raw))
        except (TypeError, ValueError):
            raise ScenarioError(f"unit.spawn({defname}) failed: {raw!r}")
        if uid < 0:
            raise ScenarioError(f"unit.spawn({defname}) returned {uid}")
        uids.append(uid)
    time.sleep(1.5)   # let the unit thread materialize inventories
    for uid, defname in zip(uids, ROSTER):
        if defname == "acolyte" and not clear_find_water(port, uid):
            raise ScenarioError(f"unit {uid} never got AI state")
    return uids


# ---------------------------------------------------------------------
# Checkpoints
# ---------------------------------------------------------------------
def snapshot(port: int, uid: int) -> dict:
    snap = send_json(port, f"return _SCEN_snap({uid})", timeout=20.0)
    if not isinstance(snap, dict):
        raise ScenarioError(f"checkpoint read for unit {uid} failed: {snap!r}")
    return snap


def checkpoint(port: int, label: str, uids: list[int], t0: float) -> dict:
    return {
        "label": label,
        "elapsed": time.time() - t0,
        "units": {uid: snapshot(port, uid) for uid in uids},
    }


def _as_list(v):
    """The console renders an EMPTY Lua table as `{}` (an object), so a
    wound/inventory list can come back as either `[]` or `{}`."""
    if isinstance(v, list):
        return v
    if isinstance(v, dict):
        return list(v.values())
    return []


def _num(v, default=None):
    try:
        return float(v)
    except (TypeError, ValueError):
        return default


def fmt_inventory(snap: dict) -> str:
    counts: dict[str, int] = {}
    fills: dict[str, float] = {}
    for item in _as_list(snap.get("inventory")):
        if not isinstance(item, dict):
            continue
        name = item.get("defName", "?")
        counts[name] = counts.get(name, 0) + 1
        fill = _num(item.get("currentFill"), 0.0) or 0.0
        if fill > 0:
            fills[name] = fills.get(name, 0.0) + fill
    if not counts:
        return "(empty)"
    parts = []
    for name in sorted(counts):
        piece = f"{name}x{counts[name]}" if counts[name] > 1 else name
        if name in fills:
            piece += f" (fill {fills[name]:.2f})"
        parts.append(piece)
    return ", ".join(parts)


def fmt_wounds(snap: dict) -> str:
    wounds = _as_list(snap.get("wounds"))
    if not wounds:
        return "none"
    parts = []
    for w in wounds:
        if not isinstance(w, dict):
            continue
        sev = _num(w.get("severityEffective", w.get("severity")), 0.0) or 0.0
        # bandage is the residual seep factor: 1.0 = untreated, <1 dressed.
        seep = _num(w.get("bandage"), 1.0)
        dressing = w.get("dressing") or ""
        tag = f"{w.get('part', '?')} {w.get('kind', '?')} sev {sev:.2f}"
        if seep is not None and seep < 1.0:
            tag += f", dressed (seep {seep:.2f}"
            tag += f", {dressing})" if dressing else ")"
        else:
            tag += ", undressed"
        parts.append(tag)
    return "; ".join(parts)


def fmt_body(snap: dict) -> str:
    stats = snap.get("stats") if isinstance(snap.get("stats"), dict) else {}
    parts = []
    for key, maxkey in BODY_STATS:
        cur, mx = _num(stats.get(key)), _num(stats.get(maxkey))
        if cur is None:
            continue
        parts.append(f"{key} {cur:.1f}/{mx:.1f}" if mx else f"{key} {cur:.1f}")
    blood = snap.get("blood")
    if isinstance(blood, dict):
        cur, mx = _num(blood.get("current")), _num(blood.get("max"))
        rate = _num(blood.get("bleedRate"), 0.0) or 0.0
        if cur is not None:
            parts.append(f"blood {cur:.2f}"
                         + (f"/{mx:.2f} L" if mx else " L")
                         + (f" (bleeding {rate:.3f} L/s)" if rate > 0 else ""))
    pain = _num(snap.get("pain"))
    if pain is not None:
        parts.append(f"pain {pain:.2f}")
    return ", ".join(parts) if parts else "(no body stats)"


def print_unit(uid: int, snap: dict, indent: str = "    ") -> None:
    if snap.get("gone"):
        print(f"{indent}unit {uid}: GONE (destroyed or never spawned)")
        return
    name = snap.get("name") or snap.get("displayName") or snap.get("defName")
    gx, gy = _num(snap.get("gridX"), 0.0), _num(snap.get("gridY"), 0.0)
    gz = _num(snap.get("gridZ"), 0.0)
    carry, cap = _num(snap.get("carrying"), 0.0), _num(snap.get("capacity"))
    pct = f" ({100.0 * carry / cap:.0f}%)" if cap else ""
    print(f"{indent}#{uid} {name} [{snap.get('defName')}]")
    print(f"{indent}  position   ({gx:.2f}, {gy:.2f}, z {gz:.0f})  "
          f"pose {snap.get('pose')}  anim {snap.get('anim') or '-'}  "
          f"speed {_num(snap.get('moveSpeed'), 0.0):.2f}"
          + ("  KNOCKED DOWN" if snap.get("knockedDown") else ""))
    print(f"{indent}  carrying   {carry:.1f}"
          + (f" / {cap:.1f} kg{pct}" if cap else " kg"))
    print(f"{indent}  inventory  {fmt_inventory(snap)}")
    print(f"{indent}  body       {fmt_body(snap)}")
    print(f"{indent}  injuries   {fmt_wounds(snap)}")
    treating = snap.get("treating")
    print(f"{indent}  activity   action {snap.get('action') or '-'}"
          f"  role {snap.get('role') or '-'}"
          f"  commanded {'yes' if snap.get('commanded') else 'no'}"
          f"  treating {treating if treating else '-'}")


def print_checkpoint(cp: dict) -> None:
    print(f"\n  -- checkpoint: {cp['label']}  (t+{cp['elapsed']:.1f}s) --")
    for uid, snap in cp["units"].items():
        print_unit(uid, snap)


# ---------------------------------------------------------------------
# Movement
# ---------------------------------------------------------------------
def command_move(port: int, uids: list[int], tx: float, ty: float) -> None:
    """Issue the real player move order (`unitAi.commandMove`, what the
    right-click handler calls) so the AI keeps ticking and treats the
    route as a candidate action it resumes after interrupts."""
    for uid in uids:
        send(port,
             f"local ai = require('scripts.unit_ai'); "
             f"local mv = require('scripts.movement_speed'); "
             f"ai.commandMove({uid}, {tx}, {ty}, mv.ordered({uid})); "
             f"return 'ok'")


def _lua_uid_list(uids: list[int]) -> str:
    return "{" + ",".join(str(u) for u in uids) + "}"


def poll_positions(port: int, uids: list[int]) -> dict[int, tuple]:
    """One round trip for every unit's position + pose. Also clears any
    stray auto-pause: a `unit_warning` notification can pause the whole
    sim (config/notifications), which would freeze the scenario."""
    raw = send(port,
               "engine.setPaused(false); local o = {}; "
               "for _, u in ipairs(" + _lua_uid_list(uids) + ") do "
               "local i = unit.getInfo(u); "
               "if i then o[#o+1] = u .. ':' .. string.format('%.3f,%.3f,%.3f', "
               "i.gridX, i.gridY, i.gridZ) .. ':' .. tostring(unit.getPose(u)) "
               "else o[#o+1] = u .. ':gone' end end; "
               "return table.concat(o, ';')")
    out: dict[int, tuple] = {}
    for field in raw.split(";"):
        bits = field.split(":")
        if len(bits) == 2 and bits[1] == "gone":
            out[int(bits[0])] = (None, None, None, "gone")
            continue
        if len(bits) != 3:
            continue
        x, y, z = (float(v) for v in bits[1].split(","))
        out[int(bits[0])] = (x, y, z, bits[2])
    return out


def walk_leg(port: int, uids: list[int], tx: float, ty: float,
             budget: float, observations: list[str], leg: str) -> dict:
    """Send the party to (tx, ty) and poll until everyone arrives or the
    leg's time budget runs out. Never fails the run — a unit that stalls,
    dies or is diverted is an OBSERVATION."""
    command_move(port, uids, tx, ty)
    deadline = time.time() + budget
    next_reissue = time.time() + 20.0
    arrived: dict[int, bool] = {u: False for u in uids}
    settled: dict[int, bool] = {u: False for u in uids}   # arrived or lost
    last: dict[int, tuple] = {}
    while time.time() < deadline:
        last = poll_positions(port, uids)
        for uid in uids:
            px, py, _pz, pose = last.get(uid, (None, None, None, "gone"))
            if px is None or pose in ("gone", "dead"):
                settled[uid] = True     # nothing more to wait for
                continue
            if ((px - tx) ** 2 + (py - ty) ** 2) ** 0.5 <= ARRIVAL_TILES:
                arrived[uid] = settled[uid] = True
        if all(settled.values()):
            break
        if time.time() >= next_reissue:
            # The AI drops a commanded task after its own 60 s timeout
            # (unit_ai_core TASK_TIMEOUT_SEC) or when an interrupt wins;
            # re-issuing keeps the route the standing order.
            pending = [u for u in uids if not settled[u]]
            if pending:
                command_move(port, pending, tx, ty)
            next_reissue = time.time() + 20.0
        time.sleep(0.5)
    for uid in uids:
        px, py, _pz, pose = last.get(uid, (None, None, None, "gone"))
        where = "an unknown position" if px is None else f"({px:.1f}, {py:.1f})"
        if pose == "gone":
            observations.append(
                f"unit {uid} no longer exists at the end of {leg}")
        elif pose == "dead":
            observations.append(
                f"unit {uid} is DEAD at the end of {leg} "
                f"(last seen at {where})")
        elif not arrived[uid]:
            short = ("" if px is None else
                     f"{((px - tx) ** 2 + (py - ty) ** 2) ** 0.5:.1f} tiles "
                     f"short at ")
            observations.append(
                f"unit {uid} did not reach {leg} target ({tx:.0f}, {ty:.0f}) "
                f"within {budget:.0f}s — stopped {short}{where}, pose {pose}")
    return arrived


# ---------------------------------------------------------------------
# Inventory transfer (the existing engine path the fetch AI uses)
# ---------------------------------------------------------------------
def transfer(port: int, frm: int, to: int, defname: str, count: int = 1) -> int:
    """Move up to `count` instances of `defname` between two units via
    `unit.transferItemToUnit` — the atomic all-or-nothing engine path
    acolytes already use to pull stock off the technomule. Returns how
    many actually moved.

    The engine verb deliberately has no capacity check (the Lua caller
    gates, the same way pickup and the fetch AI do), so the loop stops
    at the receiver's carrying capacity rather than silently burying an
    acolyte under an encumbrance penalty the route would then measure."""
    raw = send(port,
               f"local n = 0; for _ = 1, {count} do "
               f"local w = unit.getCarryingWeight({to}) or 0; "
               f"local cap = unit.getStat({to}, 'carrying_capacity'); "
               f"if cap and w >= cap then break end; "
               f"if unit.transferItemToUnit({frm}, {to}, '{defname}') "
               f"then n = n + 1 else break end end; return n")
    return int(_num(raw, 0) or 0)


# ---------------------------------------------------------------------
# Scenario: expedition
# ---------------------------------------------------------------------
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
        # point; it never travels.
        moved = []
        for uid in party:
            moved.append((uid, "rations", transfer(port, mule, uid,
                                                   "rations", 3)))
        moved.append((party[0], "first_aid_kit",
                      transfer(port, mule, party[0], "first_aid_kit", 1)))
        for uid, name, n in moved:
            if n == 0:
                observations.append(
                    f"no {name} could be transferred from the mule "
                    f"({mule}) to acolyte {uid}")

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
            f"{n}x {name} -> {uid}" for uid, name, n in moved))
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


# ---------------------------------------------------------------------
# Scenario: first-aid
# ---------------------------------------------------------------------
def kit_state(port: int, uids: list[int]) -> str:
    """Where the first-aid kit is now and what is left in it."""
    raw = send_json(port,
                    "local o = {}; "
                    "for _, u in ipairs(" + _lua_uid_list(uids) + ") do "
                    "for _, it in ipairs(unit.getInventory(u) or {}) do "
                    "if it.defName == 'first_aid_kit' then "
                    "o[#o+1] = {holder = u, "
                    "contents = unit.getItemContents(u, 'first_aid_kit', "
                    "it.instanceId)} end end end; return o", timeout=20.0)
    entries = _as_list(raw)
    if not entries:
        return "no first-aid kit is held by any roster unit"
    out = []
    for entry in entries:
        if not isinstance(entry, dict):
            continue
        rows = _as_list(entry.get("contents"))
        items = ", ".join(
            f"{r.get('defName', '?')}x{int(_num(r.get('count'), 0) or 0)}"
            for r in rows if isinstance(r, dict)) or "(empty)"
        out.append(f"held by unit {entry.get('holder')}: {items}")
    return "; ".join(out)


def descend_and_fall(port: int, scout: int, tx: float, ty: float,
                     budget: float, observations: list[str]) -> tuple:
    """Send the scout off the ridge and watch tightly for the landing.

    A shallow fall opens a live bleed measured in whole litres per
    second, so this polls as fast as the console allows rather than on
    the leisurely walk_leg cadence — otherwise the injury is only ever
    observed after the patient has already bled out, and there is no
    treatment situation left to stage."""
    command_move(port, [scout], tx, ty)
    deadline = time.time() + budget
    pose, nwounds, where = "unknown", 0, "unknown"
    while time.time() < deadline:
        raw = send(port,
                   "engine.setPaused(false); "
                   f"local i = unit.getInfo({scout}); "
                   "if not i then return '0|gone|unknown' end; "
                   f"local w = unit.getWounds({scout}); "
                   "return (w and #w or 0) .. '|' .. "
                   f"tostring(unit.getPose({scout})) .. '|' .. "
                   "string.format('(%.2f, %.2f, z %.0f)', i.gridX, i.gridY, "
                   "i.gridZ)")
        bits = raw.split("|")
        if len(bits) == 3:
            nwounds, pose, where = int(_num(bits[0], 0) or 0), bits[1], bits[2]
        if nwounds > 0 or pose in ("dead", "gone"):
            break
        time.sleep(0.2)
    if pose == "gone":
        observations.append("the scout no longer exists after the descent")
    elif nwounds == 0:
        observations.append(
            "the descent produced no wound at all within "
            f"{budget:.0f}s — the fall either did not happen or was survived "
            f"unscathed (scout at {where}, pose {pose})")
    return pose, nwounds, where


def dressed(port: int, uid: int) -> bool:
    """True once any wound carries a dressing (bandage < 1 = seep cut)."""
    raw = send(port,
               f"local ws = unit.getWounds({uid}); "
               "if type(ws) ~= 'table' then return 'no' end; "
               "for _, w in ipairs(ws) do "
               "if (w.bandage or 1) < 1 or (w.dressing or '') ~= '' "
               "then return 'yes' end end; return 'no'")
    return raw == "yes"


def run_first_aid(port: int) -> int:
    proc = boot(port, log=LOG, label="first-aid engine")
    t0 = time.time()
    observations: list[str] = []
    checkpoints: list[dict] = []
    try:
        bootstrap(port)
        print("building the arena ridge ...")
        send(port, "require('scripts.movement_arena'); return 'ok'")
        send(port, "return require('scripts.movement_arena')"
                   ".buildCourse('flat')", timeout=90.0)
        page = send(port, "return require('scripts.movement_arena').page")
        send(port,
             f"require('scripts.movement_arena').plateau({FA_RIDGE_X1}, "
             f"{FA_RIDGE_Y1}, {FA_RIDGE_X2}, {FA_RIDGE_Y2}, {FA_RIDGE_H}, "
             f"{FA_LOAM}); return 'built'", timeout=180.0)
        # The tile edits are queued to the world thread; wait for the
        # ridge to actually stand before spawning onto it.
        top = None
        for _ in range(60):
            time.sleep(0.5)
            raw = send(port, f"local _s, t = world.getTerrainAt"
                             f"({FA_SCOUT_TILE[0]}, {FA_SCOUT_TILE[1]}); "
                             f"return t")
            top = _num(raw)
            if top is not None and top >= FA_RIDGE_H:
                break
        if top is None or top < FA_RIDGE_H:
            raise ScenarioError(
                f"the arena ridge never reached z {FA_RIDGE_H} "
                f"(got {top}) — cannot stage a real fall")

        camp_tiles = [FA_SCOUT_TILE,                        # the scout
                      FA_MEDIC_TILE,                        # the medic
                      (FA_CAMP_X, 1), (FA_CAMP_X, -1),
                      (FA_CAMP_X + 1, 0),                   # 3 more acolytes
                      (FA_CAMP_X + 1, 1)]                   # technomule
        uids = spawn_roster(port, page, camp_tiles)
        acolytes, mule = uids[:5], uids[5]
        scout, medic = acolytes[0], acolytes[1]

        n = transfer(port, mule, scout, "first_aid_kit", 1)
        if n != 1:
            raise ScenarioError(
                "the mule's stocked first-aid kit could not be moved onto "
                "the expedition acolyte via unit.transferItemToUnit")
        kit_before = kit_state(port, uids)
        checkpoints.append(checkpoint(port, "kit issued, before the fall",
                                      [scout, medic, mule], t0))

        print("  sending the scout off the ridge ...")
        pose, nwounds, landing = descend_and_fall(
            port, scout, FA_CAMP_X, 0, budget=90.0, observations=observations)

        # Treat the moment the injury lands. The medic AI is live and its
        # own treat_ally path (fetch the kit off the patient, close, dress)
        # is reported below, but a fall opens a multi-litre-per-second
        # bleed: waiting out the AI's fetch-and-walk means observing a
        # corpse. So the runner administers first aid immediately through
        # the same engine verb the AI itself calls, drawing on the kit we
        # issued to the scout (kit owner = the patient).
        direct = None
        if nwounds > 0 and pose not in ("dead", "gone"):
            direct = send_json(
                port, f"return unit.treatBleeding({medic}, {scout}, {scout})",
                timeout=20.0)
        elif pose == "dead":
            observations.append(
                "the scout was already dead when the injury was first "
                "observed, so no treatment was administered")
        checkpoints.append(checkpoint(port, "after the fall and first aid",
                                      [scout, medic], t0))

        # Whatever the medic AI does on its own is a separate observation.
        print("  observing the medic AI ...")
        ai_engaged = False
        deadline = time.time() + 30.0
        while time.time() < deadline:
            send(port, "engine.setPaused(false)", expect_result=False)
            claim = send(port,
                         "local ai = package.loaded['scripts.unit_ai']; "
                         f"local s = ai and ai.getState({medic}); "
                         "if s and s.treatClaim then return "
                         "tostring(s.treatClaim.patient) end; return 'none'")
            if claim == str(scout):
                ai_engaged = True
                break
            if send(port, f"return unit.getPose({scout})") in ("dead", "nil"):
                break
            time.sleep(1.0)
        observations.append(
            f"the medic AI on acolyte {medic} claimed the scout as a patient"
            if ai_engaged else
            f"the medic AI on acolyte {medic} never claimed the scout within "
            f"30s of the fall")
        if dressed(port, scout):
            observations.append("the scout ends the run with a dressed wound")
        observations.append(
            f"the scout's final pose is "
            f"{send(port, f'return unit.getPose({scout})')}")
        checkpoints.append(checkpoint(port, "final state",
                                      [scout, medic, mule], t0))

        print("\n" + "=" * 72)
        print("FIRST-AID SCENARIO REPORT")
        print("=" * 72)
        print(f"world          movement_arena page '{page}' with a "
              f"{FA_RIDGE_H}-z ridge at x {FA_RIDGE_X1}..{FA_RIDGE_X2}, "
              f"y {FA_RIDGE_Y1}..{FA_RIDGE_Y2}")
        print(f"roster         {len(acolytes)} acolytes + 1 technomule "
              f"(uids {uids}), player faction")
        print(f"scout          acolyte {scout} (fell), medic acolyte {medic}")
        print(f"supply point   technomule {mule}, stationary at camp")
        print("kit issue      1x first_aid_kit moved mule -> scout via "
              "unit.transferItemToUnit")
        print(f"kit before     {kit_before}")
        print(f"kit after      {kit_state(port, uids)}")
        print(f"landing        {landing}, pose {pose}, {nwounds} wound(s); "
              f"expected near {FA_LANDING}")
        if direct is None:
            print("treatment      none administered — see the observations")
        elif isinstance(direct, dict):
            print(f"treatment      unit.treatBleeding(medic {medic}, "
                  f"patient {scout}, kit owner {scout}) -> "
                  f"ok={direct.get('ok')}, "
                  f"part={direct.get('part')}, kind={direct.get('kind')}, "
                  f"method={direct.get('method')}, "
                  f"bandagesUsed={direct.get('bandagesUsed')}, "
                  f"attempts={direct.get('attempts')}, "
                  f"seep={direct.get('seep')}, "
                  f"message={direct.get('message')!r}")
        else:
            print(f"treatment      the call returned {direct!r}")
        for cp in checkpoints:
            print_checkpoint(cp)
        print("\n  -- observations --")
        for line in observations or ["nothing out of the ordinary"]:
            print(f"    * {line}")
        print("\n  NOTE: this report is an observation, not a verdict. "
              "Whether the\n  treatment succeeded or the scout survived is "
              "NOT asserted here.")
        return 0
    finally:
        quit_engine(port, proc)


# ---------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------
SCENARIOS = {
    "expedition": (
        "Five acolytes + one technomule on a fixed-seed world: two "
        "acolytes are provisioned off the stationary mule and walk a "
        "fixed out-and-back route, reporting inventory, carrying state, "
        "hunger/hydration/exhaustion, injuries, treatment activity and "
        "position at every waypoint.",
        run_expedition),
    "first-aid": (
        "The same starting roster on a repeatable arena: the mule's "
        "stocked first-aid kit is moved onto the expedition acolyte, who "
        "takes a real fall; reports the injury, the treatment attempt or "
        "outcome, the kit's remaining contents and the final unit state.",
        run_first_aid),
}


def main() -> int:
    ap = argparse.ArgumentParser(
        description="Manual first-expedition gameplay scenarios (#925). "
                    "Diagnostics only — deliberately outside CI, and the "
                    "exit status reports setup/runtime failure, never a "
                    "gameplay-balance verdict.")
    ap.add_argument("--list", action="store_true",
                    help="list the available scenarios and exit")
    ap.add_argument("--test", metavar="NAME",
                    help="scenario to run (see --list)")
    ap.add_argument("--port", type=int, default=DEFAULT_PORT,
                    help=f"debug-console port (default {DEFAULT_PORT}; "
                         f"never use 8008, the GUI's port)")
    args = ap.parse_args()

    if args.list:
        for name in sorted(SCENARIOS):
            print(f"{name}\n    {SCENARIOS[name][0]}")
        return 0
    if not args.test:
        print("error: no test selected — pass --test <name> or --list "
              f"(known tests: {', '.join(sorted(SCENARIOS))})",
              file=sys.stderr)
        return 2
    if args.test not in SCENARIOS:
        print(f"error: unknown test {args.test!r} "
              f"(known tests: {', '.join(sorted(SCENARIOS))}; use --list)",
              file=sys.stderr)
        return 2

    # Every runtime resource family is loaded by cwd-relative path, and
    # `cabal run` needs the project root, so anchor both to the checkout
    # this script lives in rather than wherever it was invoked from.
    os.chdir(REPO_ROOT)
    try:
        return SCENARIOS[args.test][1](args.port)
    except ScenarioError as exc:
        print(f"\nSETUP/RUNTIME FAILURE: {exc}", file=sys.stderr)
        print(f"engine log: {LOG}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
