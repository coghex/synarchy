#!/usr/bin/env python3
"""Shared support for the manual gameplay scenarios (#925, #2151).

The one owner of everything the two scenarios behind
``tools/gameplay_scenarios.py`` have in common: the gameplay catalog and
script bootstrap, the installed unit-snapshot reader, the simulation
hold and its stopped-baseline plumbing (#1218), the real starting
roster, unit snapshots and checkpoint construction, the shared
checkpoint/body/inventory/wound formatting, movement commands, position
polling and waypoint walking, and the capacity-gated item transfer
(#1212) with its provisioning-load report. ``ScenarioError``,
``DEFAULT_PORT``, ``LOG`` and the arrival tolerance live here too: the
façade and both scenario owners read them, and each must have exactly
one definition — two ``ScenarioError`` classes would leave the façade's
``except`` blind to one owner's setup failures, and a per-module ``LOG``
would let the path the failure diagnostic prints drift from the path the
scenario actually wrote to.

Dependencies run ONE way. Both scenario owners
(``gameplay_scenarios_expedition.py``, ``gameplay_scenarios_first_aid.py``)
and the façade import this module; this module imports neither of them
and knows nothing about any scenario. A helper only one scenario
currently calls (``walk_leg``) still belongs here when it is built from
the shared ``command_move``/``poll_positions``/``ARRIVAL_TILES`` trio.
Leading-underscore helpers (``_as_list``, ``_num``, ``_lua_uid_list``)
are shared facts as well, imported by the owners rather than copied.

THIS IS NOT A BEHAVIOR PROBE AND NOT A CI GATE — see the façade's
docstring. There is no command line here; run a scenario through
``python3 tools/gameplay_scenarios.py --test <name>``.
"""
from __future__ import annotations

import glob
import sys
import time
from pathlib import Path
from typing import NamedTuple

sys.path.insert(0, str(Path(__file__).resolve().parent))
from probelib import send, send_json, clear_find_water  # noqa: E402

DEFAULT_PORT = 9925
LOG = "/tmp/gameplay_scenarios_engine.log"

# The real starting party (scripts/building_spawn.lua's acolyte_portal
# roster): five acolytes, then the technomule that hauls the stock.
ROSTER = ["acolyte"] * 5 + ["technomule"]
PLAYER_FACTION = "player"

# The arrival tolerance every "did it get there" test shares: waypoint
# walking, the first-aid pre-fall baseline and its report all read it.
ARRIVAL_TILES = 0.8           # matches the AI's own 0.6 + a little slack

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


# ---------------------------------------------------------------------
# Simulation hold (#1218)
# ---------------------------------------------------------------------
# `engine.setPaused` is the ONLY real hold. `unit.setFrozen` is not one:
# it merely stops the render-facing snapshot while the sim keeps moving
# the unit, so a "frozen" unit walks on and `unit.getInfo` reports where
# it used to be (CLAUDE.md's expedition-loop notes). Movement ticks are
# gated on the engine pause flag (src/Unit/Thread.hs) and the utility AI
# returns early while it is set (scripts/unit_ai.lua), so a stopped
# simulation is the only window in which a baseline read is coherent.
#
# What still works under the hold is what makes a paused setup possible:
# the unit thread drains its command queue regardless of the flag, so
# spawns and `unit.transferItemToUnit` complete, and every read verb is
# a plain query.
def is_paused(port: int) -> bool:
    return send(port, "return tostring(engine.isPaused())") == "true"


def hold_simulation(port: int) -> None:
    """Stop the simulation and prove it stopped, rather than assuming the
    request took."""
    send(port, "engine.setPaused(true); return 'ok'")
    if not is_paused(port):
        raise ScenarioError(
            "the simulation would not stop — engine.setPaused(true) did not "
            "take, so no baseline can be captured under a controlled "
            "simulation")


def release_simulation(port: int) -> None:
    send(port, "engine.setPaused(false); return 'ok'")


def _retire_find_water_stopped(port: int, uid: int) -> str:
    """Retire a spawned acolyte's standing `find_water` goal while the
    simulation is stopped.

    probelib's `clear_find_water` polls `unitAi.getState`, which only
    becomes non-nil once the AI has TICKED the unit — and the AI tick
    returns early while paused, so that helper can never succeed under a
    hold. Reaching for the AI's own state constructor instead retires the
    goal before the unit is ever ticked: `seedInitialGoal` skips a unit
    whose `goalStatus` is already non-empty, so `find_water` is never
    handed out at all rather than being handed out and then withdrawn.

    Returns the goal's resulting status for the caller to check."""
    return send(port,
                "local core = require('scripts.unit_ai_core'); "
                f"local s = core.ensureState({uid}); "
                "if type(s) ~= 'table' then return 'nostate' end; "
                "core.markGoalAccomplished(s, 'find_water'); "
                "return tostring((s.goalStatus or {}).find_water)")


def spawn_roster(port: int, page: str, tiles, hold: bool = False) -> list[int]:
    """Spawn the real starting party at `tiles` ([(x, y), ...], one per
    ROSTER entry) as PLAYER-faction units, the same way the acolyte
    portal spawns them (scripts/building_spawn.lua) — engine-side spawn
    applies each def's real starting_inventory, including the mule's
    pre-stocked first-aid kit.

    A fresh acolyte's standing `find_water` goal is retired: its search
    utility out-competes a commanded route and can walk a scout off a
    cliff, so every probe in tools/ quiets it. That is a deliberate
    scenario condition, and the report says so.

    `hold=False` (the default, and what `expedition` uses) keeps the
    historical LIVE setup: the roster materializes with the simulation
    running and the goal is cleared by polling for each acolyte's first
    AI tick. `hold=True` stops the simulation before the first spawn and
    RETURNS WITH IT STILL STOPPED, so the caller owns the release — the
    whole materialize-and-quiet window then runs with no ambient AI, and
    nothing can move or injure a unit before the caller's baseline."""
    if len(tiles) != len(ROSTER):
        raise ScenarioError("roster/tile count mismatch")
    if hold:
        hold_simulation(port)
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
        if defname != "acolyte":
            continue
        if hold:
            status = _retire_find_water_stopped(port, uid)
            if status != "accomplished":
                raise ScenarioError(
                    f"unit {uid}'s find_water goal could not be retired under "
                    f"the simulation hold (goal status {status!r})")
        elif not clear_find_water(port, uid):
            raise ScenarioError(f"unit {uid} never got AI state")
    if hold and not is_paused(port):
        # Nothing here unpauses, but a notification category configured
        # to pause/unpause could; the roster is only usable as a baseline
        # if the hold survived the whole setup.
        raise ScenarioError(
            "the simulation restarted during roster setup — the spawned "
            "party is no longer a controlled baseline")
    return uids


# ---------------------------------------------------------------------
# Checkpoints
# ---------------------------------------------------------------------
def snapshot(port: int, uid: int) -> dict:
    snap = send_json(port, f"return _SCEN_snap({uid})", timeout=20.0)
    if not isinstance(snap, dict):
        raise ScenarioError(f"checkpoint read for unit {uid} failed: {snap!r}")
    return snap


def checkpoint(port: int, label: str, uids: list[int], t0: float,
               snaps: dict[int, dict] | None = None,
               paused: bool | None = None) -> dict:
    """Read every unit in `uids` into one checkpoint record.

    `snaps` supplies already-taken snapshots, so a read that has already
    been VALIDATED under a stopped simulation is the very one reported
    rather than a second, unvalidated re-read. `paused`, when given,
    records the `engine.isPaused()` state the checkpoint was captured
    under, so the report demonstrates the hold instead of implying it."""
    snaps = snaps or {}
    cp = {
        "label": label,
        "elapsed": time.time() - t0,
        "units": {uid: (snaps[uid] if uid in snaps else snapshot(port, uid))
                  for uid in uids},
    }
    if paused is not None:
        cp["paused"] = paused
    return cp


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
    paused = cp.get("paused")
    sim = "" if paused is None else (
        f"  [simulation {'STOPPED' if paused else 'RUNNING'}: "
        f"engine.isPaused() == {'true' if paused else 'false'}]")
    print(f"\n  -- checkpoint: {cp['label']}  (t+{cp['elapsed']:.1f}s){sim} --")
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
class TransferResult(NamedTuple):
    """What one `transfer` call actually did.

    `code` is `'ok'` when every requested instance moved; otherwise it
    names why the loop stopped and `detail` is the human-readable
    sentence the report prints. `moved` is meaningful either way — a
    partial move reports the count AND the reason the next instance was
    turned away, so provisioning two of three rations is as legible as
    provisioning none."""
    moved: int
    code: str
    detail: str

    @property
    def refused(self) -> bool:
        """True when the loop stopped short of the requested count."""
        return self.code != "ok"

    @property
    def observational(self) -> bool:
        """True when the refusal is a GAMEPLAY outcome (the receiver has
        no room) rather than a setup/runtime failure. The runner's exit
        status reports the latter only, so a caller that treats a failed
        transfer as fatal must consult this first."""
        return self.code in ("capacity", "no-capacity")


def transfer(port: int, frm: int, to: int, defname: str,
             count: int = 1) -> TransferResult:
    """Move up to `count` instances of `defname` between two units via
    `unit.transferItemToUnit` — the atomic all-or-nothing engine path
    acolytes already use to pull stock off the technomule.

    **No successful move here can leave the receiver over its carrying
    capacity** (#1212). The engine verb deliberately has no capacity
    check (the Lua caller gates, the same way pickup and the fetch AI
    do), and asking only whether the receiver is ALREADY full is not
    that gate: a receiver a gram below its cap would accept one more
    indivisible item and finish over it, and since encumbrance scales
    movement speed (#305) the overshoot then contaminates the very route
    measurements this runner exists to produce.

    So each iteration picks a CONCRETE source instance — the first
    `defname` match in `unit.getInventory(frm)`, which is exactly the
    instance the verb would move on its own — and applies the strict
    transfer policy's own prospective rule to it:
    `unit.getCarryingWeight(to) + that instance's weight <=
    unit.getStat(to, 'carrying_capacity')`. Both readings are the pair
    `Unit.Transfer`'s `fits` uses (`unit.getInventory`'s `weight` is the
    full recursive `itemTotalWeight` — case, fill and nested contents;
    `getStat` is modifier-applied), and the chosen instance's own
    `instanceId` is passed to the verb so the item that was weighed is
    the item that moves. Capacity is re-read every iteration, so a
    3-ration request stops at whichever ration no longer fits.

    A receiver that is already over capacity simply refuses the next
    item and says so; nothing here unloads inventory it did not put
    there. The strict `unit.checkTransfer`/`unit.commitTransfer` path
    computes the same rule engine-side, but additionally requires
    Chebyshev reach <= 1 (`src/Unit/Transfer.hs`) — and the first-aid
    scenario provisions its scout across the arena ridge — so this
    scenario keeps the legacy verb and does the projection itself.

    Returns a `TransferResult`; a capacity refusal is a gameplay
    observation, never a `ScenarioError`."""
    lua = (
        "local moved, code, detail = 0, 'ok', ''; "
        f"for _ = 1, {count} do "
        f"local inv = unit.getInventory({frm}); "
        "local iid, iw = nil, nil; "
        "if inv ~= nil then for _, it in ipairs(inv) do "
        f"if it.defName == '{defname}' then "
        "iid, iw = it.instanceId, (it.weight or 0); break end end end; "
        "if iid == nil then code = 'source-empty'; detail = "
        f"'unit {frm} holds no {defname} to hand over'; break end; "
        f"local cap = unit.getStat({to}, 'carrying_capacity'); "
        f"local carried = unit.getCarryingWeight({to}) or 0; "
        "if cap == nil or cap <= 0 then code = 'no-capacity'; detail = "
        f"string.format('unit {to} reports no usable carrying_capacity "
        "(%s), so no item can be shown to fit', tostring(cap)); break end; "
        "if carried + iw > cap then code = 'capacity'; detail = string.format("
        f"'the next {defname} weighs %.2f kg and unit {to} already carries "
        "%.2f kg of its %.2f kg capacity, so accepting it would leave it "
        "%.2f kg over', iw, carried, cap, carried + iw - cap); break end; "
        f"if unit.transferItemToUnit({frm}, {to}, '{defname}', iid) then "
        "moved = moved + 1; else code = 'engine-refused'; detail = "
        "string.format('unit.transferItemToUnit refused instance %d of "
        f"{defname}', iid); break end end; "
        "return moved .. '|' .. code .. '|' .. detail"
    )
    raw = send(port, lua, timeout=20.0)
    bits = raw.split("|", 2)
    if len(bits) != 3:
        raise ScenarioError(
            f"provisioning {defname} from unit {frm} to unit {to} replied "
            f"{raw!r}, which is not a moved|code|detail result")
    return TransferResult(int(_num(bits[0], 0) or 0), bits[1], bits[2])


def provisioning_load(port: int, uid: int) -> str:
    """The receiver's post-provisioning load against its capacity, read
    from the same pair `transfer` gates on — so the report states the
    guarantee outright instead of leaving it to be inferred from the
    checkpoint below."""
    raw = send(port,
               f"local w = unit.getCarryingWeight({uid}) or 0; "
               f"local cap = unit.getStat({uid}, 'carrying_capacity'); "
               "if cap == nil then return string.format('%.2f kg carried, "
               "no carrying_capacity stat', w) end; "
               "return string.format('%.2f / %.2f kg (%s capacity)', w, cap, "
               "(w <= cap) and 'within' or 'OVER')")
    return raw or "unreadable"
