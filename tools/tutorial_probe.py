#!/usr/bin/env python3
"""First-session tutorial integration gate (#922, phase 5 of epic #956).

The four tutorial slices each shipped their own gate: #957's tree
loading, #958's progress model, #959's evaluator predicates and #960's
HUD are covered by pure hspec groups ("Tutorial definitions",
"Tutorial progress", "Tutorial evaluation", "Tutorial HUD") plus the
GPU offscreen surface check in tools/tutorial_hud_probe.py. Every one of
those runs against stubs, injected trees, or a synthetic view model.

This probe is the missing joint: the SHIPPED data/tutorials/first_session.yaml
branch driven from end to end by real gameplay state in a real engine —

    Place portal
      -> Secure water source
        -> Prepare an expedition
             - Prepare water
             - Prepare food

— and then carried through a real save/load round trip in a SECOND
process. Nothing here injects a tree, stubs a predicate, or writes the
progress tables directly; every transition below is produced by placing
a real building, teleporting a real acolyte next to real generated
water, and moving real items in and out of a real inventory.

What it proves (issue #922 requirements 2 and 3):

  1. A session with no portal and no units starts with nothing completed
     and only the root objective revealed.
  2. Placing an acolyte_portal completes the portal objective and
     reveals "Secure water source".
  3. One acolyte DISCOVERING generated water (its own FOV scan) and
     SHARING it over the radio with a second acolyte completes the
     water-source objective. Both legs are asserted, because the share
     is the notify_allies fan-out and a single-acolyte run would never
     execute it. The share leg is made conclusive rather than
     circumstantial: the recipient is immobilized in a camp with no
     water anywhere in its own field of view (asserted against the
     engine's real `unit.getVisibleTiles`, before and after), and the
     source it ends up holding must be one of the FINDER's — so it
     cannot have discovered anything itself.
  4. Carrying >= 2 L of water checks the live water subobjective on its
     own, WITHOUT completing the composite.
  5. Carrying >= 2 L of water AND a ration on the SAME acolyte checks
     the food subobjective and permanently completes "Prepare an
     expedition".
  6. Stripping those supplies afterwards unchecks both live
     subobjectives and leaves the completed composite latched.
  7. A save/load round trip through a fresh process preserves every
     completed full objective, brings the HUD back collapsed, and
     recomputes the live subobjectives from the LOADED world's state
     (both unchecked, because the supplies were removed before saving).
  8. (#996, a separate boot) An UNSTRIPPED acolyte's default spawn kit
     latches "Prepare an expedition" and checks both its subobjectives
     before that branch is ever revealed -- reveal is gated on
     "Secure water source", which has nothing to do with carried
     supplies. The moment secure_water_source finally completes (a real
     FOV discovery, same as check 3 above), the already-latched branch
     must become observable in the checklist rather than latching and
     hiding in the same instant -- and removing the supplies afterwards
     must still uncheck the live subobjectives, keep the branch
     observable, and never touch the durable completion.

Two deliberate departures from "just play the game", both forced by the
shipped content and both documented rather than worked around:

  * The portal's automatic starting roster is suppressed
    (`building.setSpawnRemaining(bid, 0)`) and the probe spawns its own
    two-acolyte party with the SAME faction-tagged call
    scripts/building_spawn.lua makes. Every acolyte spawns with a full
    2 L canteen and two rations (data/units/acolyte.yaml
    starting_inventory), so the roster would satisfy both prepare
    subobjectives the instant it appeared and latch the composite before
    any transition could be observed — scripts/tutorial_eval.lua documents
    that as intended behavior. Shedding the spawn kit first and
    restoring it stepwise is the only way checks 4-6 above exist as
    distinct, observable events.
  * Setup runs PAUSED. scripts/tutorial_eval.lua is deliberately not
    pause-gated (evaluating is observation, not simulation) while
    scripts/unit_ai.lua is, so a paused window lets the probe change one
    gameplay fact at a time and read the tutorial's answer without the
    AI moving, drinking, or foraging underneath it. The unpaused windows
    are exactly the two that need simulation: giving the party its AI
    state, and the water discovery/share leg.

Slow (a real worldSize-64 generation plus two engine boots and a save/
load transaction) and manual-only — never a CI gate, per #922's own
requirement 6.

Usage:
  python3 tools/tutorial_probe.py
  python3 tools/tutorial_probe.py --seed 42 --size 64 --port 9424

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import glob
import os
import shutil
import sys
import time

from probelib import (boot, quit_engine, send, poll_until,
                      wait_load_published, wait_save_complete,
                      capture_request_id)

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
LOG_A = "/tmp/tutorial_probe_engine_a.log"
LOG_B = "/tmp/tutorial_probe_engine_b.log"
LOG_C = "/tmp/tutorial_probe_engine_c.log"

PAGE = "tutorial"
SLOT = "tutorial_probe_slot"

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

# How far the party camps from any water. Comfortably past the acolyte
# FOV scan radius, so a camped acolyte cannot discover the lake before
# the probe teleports it there.
CAMP_CLEARANCE = 12
CAMP_MIN_DISTANCE = 20

YAML_LOADERS = [
    ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
    ("data/infections/*.yaml", "engine.loadInfectionYaml"),
    ("data/items/*.yaml", "engine.loadItemYaml"),
    ("data/equipment/*.yaml", "engine.loadEquipmentYaml"),
    ("data/materials/*.yaml", "engine.loadMaterialYaml"),
    ("data/flora/*.yaml", "engine.loadFloraYaml"),
    ("data/units/*.yaml", "engine.loadUnitYaml"),
    ("data/buildings/*.yaml", "engine.loadBuildingYaml"),
]

#: The AI stack plus the tutorial runtime. scripts/init_loader.lua loads
#: the tutorial trio at these same z-orders in a real session; headless
#: has no loading screen, so the probe reproduces it.
SCRIPTS = [
    ("scripts/unit_stats.lua", 0.1),
    ("scripts/unit_resources.lua", 0.2),
    ("scripts/unit_ai.lua", 0.1),
    ("scripts/tutorial_progress.lua", 1.0),
    ("scripts/tutorial_eval.lua", 1.0),
    ("scripts/tutorial_hud.lua", 0.2),
]

failures: list[str] = []


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f" — {detail}" if detail and not ok else ""))
    if not ok:
        failures.append(name if not detail else f"{name} — {detail}")
    return ok


class ProbeError(RuntimeError):
    """Setup failure — the probe could not reach the state it tests."""


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


# --------------------------------------------------------------------------
# Engine bootstrap
# --------------------------------------------------------------------------
def load_yaml_dir(port: int, pattern: str, fn: str) -> None:
    for path in sorted(glob.glob(os.path.join(REPO_ROOT, pattern))):
        rel = os.path.relpath(path, REPO_ROOT)
        send(port, f"{fn}('{rel}'); return 'ok'", timeout=20.0)


def load_content(port: int) -> None:
    for pattern, fn in YAML_LOADERS:
        load_yaml_dir(port, pattern, fn)
    got = send(port, "return engine.loadTutorialDir('data/tutorials')", timeout=20.0)
    if got.strip() in ("", "nil", "false"):
        raise ProbeError(f"engine.loadTutorialDir failed: {got!r}")


def load_scripts(port: int) -> None:
    for path, z in SCRIPTS:
        send(port, f"engine.loadScript('{path}', {z}); return 'ok'", timeout=20.0)
    # The tree is fetched lazily on first use; resolve it now so a
    # content failure reports here instead of as a mystery empty panel.
    tree = send(port,
                "local t = require('scripts.tutorial_progress').ensureTree(); "
                "return t and t.id or 'nil'", timeout=15.0)
    if tree != "first_session":
        raise ProbeError(f"expected the first_session tree, got {tree!r}")


def set_paused(port: int, on: bool) -> None:
    send(port, f"engine.setPaused({'true' if on else 'false'}); "
               f"return tostring(engine.isPaused())", timeout=10.0)


def generate_world(port: int, seed: int, size: int) -> None:
    send(port, f"world.init('{PAGE}', {seed}, {size}, 3); return 'ok'",
         expect_result=False)
    send(port, "return world.waitForInit(300)", timeout=310.0)
    send(port, f"world.show('{PAGE}'); return 'ok'", expect_result=False)
    send(port, "return world.loadChunksInRegion(-3,-3,3,3)", timeout=30.0)
    send(port, "return world.waitForChunks(120)", timeout=130.0)


# --------------------------------------------------------------------------
# Geography: real generated water, and a camp far from it
# --------------------------------------------------------------------------
def find_water_tile(port: int) -> tuple[int, int]:
    """The generated lake/river tile nearest the origin, by ring search."""
    raw = send(port,
               "local best; for r = 1, 40 do for dx = -r, r do for dy = -r, r do "
               "if math.max(math.abs(dx), math.abs(dy)) == r then "
               "local f = world.getFluidAt(dx, dy); "
               "if f == 'lake' or f == 'river' then best = dx .. ',' .. dy; break end "
               "end end if best then break end end if best then break end end; "
               "return best or 'none'", timeout=60.0)
    if raw == "none":
        raise ProbeError("no lake or river tile within 40 tiles of the origin")
    gx, gy = (int(v) for v in raw.split(","))
    return gx, gy


def find_shore_tile(port: int, wx: int, wy: int) -> tuple[int, int]:
    """A dry, real-terrain tile adjacent to (wx, wy) — where the finder
    stands to see the water without standing IN it."""
    raw = send(port,
               f"for dx = -1, 1 do for dy = -1, 1 do "
               f"local x, y = {wx} + dx, {wy} + dy; "
               f"if world.getFluidAt(x, y) == nil and world.getTerrainAt(x, y) ~= nil "
               f"then return x .. ',' .. y end end end; return 'none'",
               timeout=30.0)
    if raw == "none":
        raise ProbeError(f"no dry shore tile beside the water at ({wx},{wy})")
    gx, gy = (int(v) for v in raw.split(","))
    return gx, gy


def find_camp_tile(port: int, wx: int, wy: int) -> tuple[int, int]:
    """The party's starting site: real terrain far enough from any water
    that a camped acolyte's FOV scan cannot reach it, and eligible for
    the portal.

    Portal eligibility is asked of `building.canPlaceAt`, the same
    validator `building.spawn` runs — a starting building is refused
    inside a placed location's bounds (#777), so a camp chosen on
    terrain alone can land on a ruin and fail at placement time.

    Coarse-stepped on both axes (candidates every 4 tiles, the clearance
    box sampled every 2): the acolyte FOV is a handful of tiles across,
    so a 2-tile sampling grid over a 16-tile box cannot miss water that
    a camped unit could see, and the whole scan stays a few thousand
    lookups instead of a few hundred thousand.
    """
    raw = send(port,
               f"local function dry(cx, cy) "
               f"for dx = -{CAMP_CLEARANCE}, {CAMP_CLEARANCE}, 2 do "
               f"for dy = -{CAMP_CLEARANCE}, {CAMP_CLEARANCE}, 2 do "
               f"if world.getFluidAt(cx + dx, cy + dy) ~= nil then return false end "
               f"end end; return true end; "
               f"for x = {wx} - 36, {wx} + 36, 4 do "
               f"for y = {wy} - 36, {wy} + 36, 4 do "
               f"if math.max(math.abs(x - {wx}), math.abs(y - {wy})) "
               f">= {CAMP_MIN_DISTANCE} and world.getTerrainAt(x, y) ~= nil "
               f"and dry(x, y) and building.canPlaceAt('{PORTAL_DEF}', x, y) then "
               f"return x .. ',' .. y end end end; return 'none'",
               timeout=180.0)
    if raw == "none":
        raise ProbeError("no water-free, portal-eligible camp site found "
                         "around the generated water")
    gx, gy = (int(v) for v in raw.split(","))
    return gx, gy


# --------------------------------------------------------------------------
# Party + supplies
# --------------------------------------------------------------------------
def spawn_player_acolyte(port: int, gx: int, gy: int) -> int:
    """Spawn a PLAYER-faction acolyte — the same call
    scripts/building_spawn.lua's portal roster makes. The faction matters:
    scripts/tutorial_eval.lua only counts `player` acolytes."""
    raw = send(port, f"return unit.spawn('acolyte', {gx}, {gy}, nil, 'player')",
               timeout=20.0)
    try:
        return int(float(raw))
    except (ValueError, TypeError):
        raise ProbeError(f"player-faction unit.spawn failed: {raw!r}")


def strip_supplies(port: int, uid: int) -> None:
    """Shed the spawn kit: drain every water container and destroy every
    ration. Leaves the (empty) canteen so the restore step below can
    simply refill it."""
    send(port,
         f"local inv = unit.getInventory({uid}) or {{}}; "
         f"for _, it in ipairs(inv) do "
         f"if it.holds == 'water' and (tonumber(it.currentFill) or 0) > 0 then "
         f"unit.modifyItemFillById({uid}, it.instanceId, -(tonumber(it.currentFill))) "
         f"end end; "
         f"while unit.removeItem({uid}, '{RATIONS_DEF}') do end; return 'ok'",
         timeout=20.0)


def carried(port: int, uid: int) -> tuple[float, int]:
    """(litres of water, ration count) this unit carries — measured the
    same way scripts/tutorial_eval.lua measures it."""
    raw = send(port,
               f"local l, r = 0, 0; "
               f"for _, it in ipairs(unit.getInventory({uid}) or {{}}) do "
               f"if it.holds == 'water' then l = l + (tonumber(it.currentFill) or 0) end; "
               f"if it.defName == '{RATIONS_DEF}' then r = r + 1 end end; "
               f"return l .. ',' .. r", timeout=20.0)
    litres, rations = raw.split(",")
    return float(litres), int(float(rations))


def known_water_count(port: int, uid: int) -> int:
    raw = send(port,
               f"local ai = require('scripts.unit_ai'); local s = ai.getState({uid}); "
               f"return s and #(s.knownWaterSources or {{}}) or -1", timeout=15.0)
    try:
        return int(float(raw))
    except (ValueError, TypeError):
        return -1


def water_sources(port: int, uid: int) -> set[tuple[int, int]]:
    """The tile coordinates in this unit's `knownWaterSources` memory."""
    raw = send(port,
               f"local ai = require('scripts.unit_ai'); local s = ai.getState({uid}); "
               f"if not s then return '' end; local out = {{}}; "
               f"for _, w in ipairs(s.knownWaterSources or {{}}) do "
               f"out[#out+1] = math.floor(w.x) .. ',' .. math.floor(w.y) end; "
               f"return table.concat(out, ';')", timeout=15.0)
    found = set()
    for pair in raw.split(";"):
        if "," in pair:
            x, y = pair.split(",")
            found.add((int(x), int(y)))
    return found


def pin_daylight(port: int) -> None:
    """Pin the world clock to local noon, so every sight-based check in
    this probe is deterministic.

    #1230 made `unit.getVisibleTiles` scale its radius by the page-local
    `nightPerceptionFactor`: at midnight a perception-1.0 unit sees 3
    tiles where it sees 6 at noon. Without this, whether the tutorial's
    water objective is reachable would depend on what hour the run
    happened to reach — the exact flakiness a probe must not have. Noon
    is the maximum, so pinning it here keeps the objective's difficulty
    at what it was before the night factor applied to the binary set.
    """
    send(port, "world.setTime(12, 0); return 'ok'", timeout=10.0)


def sees_water(port: int, uid: int) -> bool:
    """Whether ANY tile in this unit's own field of view is drinkable water.

    Asked of the engine's real FOV (`unit.getVisibleTiles`, which walks
    line of sight from `uiGridX/uiGridY` in the unit manager) against the
    same lake/river test scripts/unit_ai_water.lua's scan uses — not a
    distance heuristic against `awareRangeTiles`. If this is false, the
    unit's own scan cannot have produced a single entry in its memory.

    Since #1230 that FOV is night-aware — a perception-1.0 unit's radius
    halves at midnight — so this predicate depends on the world clock.
    Callers that compare it against a distance MUST pin the clock first
    (`pin_daylight`); this function deliberately reports whatever the
    current hour gives rather than papering over it, because the
    engine's answer IS the contract being checked.
    """
    raw = send(port,
               f"for _, t in ipairs(unit.getVisibleTiles({uid}) or {{}}) do "
               f"local f = world.getFluidAt(t.x, t.y); "
               f"if f == 'lake' or f == 'river' then return 'yes' end end; "
               f"return 'no'", timeout=20.0)
    return raw == "yes"


def hud_open(port: int) -> str:
    return send(port,
                "local h = package.loaded['scripts.tutorial_hud']; "
                "if not h then return 'absent' end; "
                "return tostring(h.dump().open)", timeout=15.0)


# --------------------------------------------------------------------------
# Phases
# --------------------------------------------------------------------------
def phase_baseline(port: int) -> None:
    p = progress(port)
    check("a fresh session has nothing completed", p.completed == [], str(p))
    check("only the root objective is revealed at the start",
          p.row_ids == [OBJ_PORTAL], str(p.row_ids))
    row = p.row(OBJ_PORTAL) or {}
    check("the root objective reads as incomplete and active",
          row.get("completed") is False and row.get("active") is True, str(row))


def phase_portal(port: int, gx: int, gy: int) -> int:
    valid = send(port, f"local v = building.canPlaceAt('{PORTAL_DEF}', {gx}, {gy}); "
                       f"return tostring(v)", timeout=20.0)
    if valid != "true":
        raise ProbeError(f"the camp tile ({gx},{gy}) will not take a portal: {valid}")
    raw = send(port, f"return tostring(building.spawn('{PORTAL_DEF}', {gx}, {gy}))",
               timeout=20.0)
    if raw in ("nil", "", "false"):
        raise ProbeError(f"building.spawn('{PORTAL_DEF}') failed at ({gx},{gy})")
    bid = int(float(raw))
    # Suppress the automatic starting roster (see the module docstring):
    # the probe supplies its own party so it controls the supply state.
    send(port, f"return tostring(building.setSpawnRemaining({bid}, 0))", timeout=15.0)

    p = settle(port, lambda s: s.is_completed(OBJ_PORTAL))
    check("placing an acolyte portal completes the portal objective",
          p.is_completed(OBJ_PORTAL), str(p))
    check("completing the portal objective reveals 'Secure water source'",
          OBJ_WATER in p.row_ids, str(p.row_ids))
    check("the water objective is revealed but not yet completed",
          p.is_completed(OBJ_WATER) is False, str(p))
    check("the expedition objective stays hidden behind the water objective",
          OBJ_EXPEDITION not in p.row_ids, str(p.row_ids))
    return bid


def phase_party(port: int, gx: int, gy: int) -> tuple[int, int]:
    """Two player acolytes at the camp, stripped of the spawn kit.

    Spawned while PAUSED, so scripts/unit_ai.lua has not created their AI
    state yet and the evaluator cannot see them at all — which is what
    lets the kit be shed before it is ever evaluated. One short unpaused
    window then grants the AI state the evaluator enumerates through.
    """
    finder = spawn_player_acolyte(port, gx, gy)
    mate = spawn_player_acolyte(port, gx + 1, gy)
    # Inventories materialize asynchronously on the unit thread; wait for
    # the spawn kit to actually exist before shedding it, or the strip
    # runs against an empty inventory and silently does nothing.
    for uid in (finder, mate):
        got = poll_until(20.0, lambda u=uid: carried(port, u)[1] > 0)
        if got is None:
            raise ProbeError(f"acolyte {uid} never materialized its spawn kit")
        strip_supplies(port, uid)
        litres, rations = carried(port, uid)
        check(f"acolyte {uid} sheds its spawn kit (0 L water, 0 rations)",
              litres == 0.0 and rations == 0,
              f"{litres} L, {rations} rations")

    set_paused(port, False)
    got = poll_until(30.0, lambda: known_water_count(port, finder) >= 0
                     and known_water_count(port, mate) >= 0)
    set_paused(port, True)
    if got is None:
        raise ProbeError("the spawned acolytes never received AI state")

    check("neither stripped acolyte knows a water source yet",
          known_water_count(port, finder) == 0 and known_water_count(port, mate) == 0,
          f"{known_water_count(port, finder)} / {known_water_count(port, mate)}")
    p = progress(port)
    check("a stripped party leaves the water objective incomplete",
          p.is_completed(OBJ_WATER) is False, str(p))
    check("a stripped party leaves the expedition objective incomplete",
          p.is_completed(OBJ_EXPEDITION) is False, str(p))
    return finder, mate


def phase_discover_and_share(port: int, finder: int, mate: int,
                             sx: int, sy: int) -> None:
    """The finder walks onto the shore, its FOV scan registers the water,
    and the radio broadcast hands it to the second acolyte.

    The recipient is FROZEN for the whole unpaused window. Without that
    it keeps its own active `find_water` goal and searches — over a
    120-second wait it can wander onto water and register a source by
    itself, which would satisfy "the recipient knows a source" even if
    the broadcast were completely broken. Freezing pins the published
    position `unitVisibleTiles` reads (`unit.setPos` is what moves it),
    so a recipient parked in the water-free camp cannot see, and
    therefore cannot scan, any water at all. `notify_allies`' radio
    branch writes straight into the recipient's AI state and applies no
    range or movement requirement, so the freeze costs the share leg
    nothing.
    """
    # #1230: unit.getVisibleTiles is night-aware, so both the finder's
    # "sees the pond it is standing on" and the recipient's "sees
    # nothing" now depend on the world clock. Pin it to noon — the
    # widest radius — so this phase asserts the same thing at every hour
    # a run can reach, and so a PASS on "the recipient is blind" is
    # earned by its position rather than by darkness.
    pin_daylight(port)
    blind_before = not sees_water(port, mate)
    check("the recipient starts with no water anywhere in its own field of view",
          blind_before)

    send(port, f"unit.setFrozen({mate}, true); return 'ok'", timeout=15.0)
    send(port, f"unit.setPos({finder}, {sx}, {sy}); return 'ok'", timeout=15.0)
    set_paused(port, False)
    found = poll_until(60.0, lambda: known_water_count(port, finder) > 0)
    shared = poll_until(120.0, lambda: known_water_count(port, mate) > 0)
    set_paused(port, True)
    # Read the recipient's view BEFORE unfreezing, so this reports the
    # pinned position it actually held for the whole window.
    blind_after = not sees_water(port, mate)
    send(port, f"unit.setFrozen({mate}, false); return 'ok'", timeout=15.0)

    check("the acolyte on the shore DISCOVERS the generated water source",
          found is not None, f"knownWaterSources={known_water_count(port, finder)}")
    check("the recipient never saw water itself while immobilized "
          "(so any source it holds was TOLD to it)", blind_after)
    check("the discovery is SHARED with the second acolyte over the radio",
          shared is not None, f"knownWaterSources={known_water_count(port, mate)}")
    finder_src, mate_src = water_sources(port, finder), water_sources(port, mate)
    check("the shared source is the finder's own tile, not a second discovery",
          bool(mate_src) and mate_src <= finder_src,
          f"recipient {sorted(mate_src)} not a subset of finder {sorted(finder_src)}")

    p = settle(port, lambda s: s.is_completed(OBJ_WATER))
    check("discovering and sharing water completes the water-source objective",
          p.is_completed(OBJ_WATER), str(p))
    check("completing the water objective reveals 'Prepare an expedition'",
          OBJ_EXPEDITION in p.row_ids, str(p.row_ids))
    check("the composite reveals both of its live subobjectives",
          SUB_WATER in p.row_ids and SUB_FOOD in p.row_ids, str(p.row_ids))

    # The finder drinks and refills at the lake during that unpaused
    # window, so re-shed both acolytes before the stepwise restore. This
    # is also the first live-reversal check: a subobjective that went
    # true from real drinking must go false again when the water is gone.
    for uid in (finder, mate):
        strip_supplies(port, uid)
    p = settle(port, lambda s: not s.is_checked(SUB_WATER))
    check("with no supplies carried, the water subobjective is unchecked",
          p.is_checked(SUB_WATER) is False, str(p))
    check("with no supplies carried, the food subobjective is unchecked",
          p.is_checked(SUB_FOOD) is False, str(p))
    check("an unsupplied party leaves the expedition objective incomplete",
          p.is_completed(OBJ_EXPEDITION) is False, str(p))


def phase_supplies(port: int, finder: int) -> None:
    """Restore the shed kit one item at a time and watch each answer."""
    send(port, f"return unit.modifyItemFill({finder}, '{CANTEEN_DEF}', "
               f"{EXPEDITION_WATER_L})", timeout=15.0)
    litres, _ = carried(port, finder)
    check(f"the acolyte now carries at least {EXPEDITION_WATER_L} L of water",
          litres >= EXPEDITION_WATER_L, f"{litres} L")

    p = settle(port, lambda s: s.is_checked(SUB_WATER))
    check("carrying enough water checks the live water subobjective",
          p.is_checked(SUB_WATER), str(p))
    check("water alone does NOT check the food subobjective",
          p.is_checked(SUB_FOOD) is False, str(p))
    check("water alone does NOT complete 'Prepare an expedition'",
          p.is_completed(OBJ_EXPEDITION) is False, str(p))

    send(port, f"return tostring(unit.addItem({finder}, '{RATIONS_DEF}', 0))",
         timeout=15.0)
    got = poll_until(20.0, lambda: carried(port, finder)[1] > 0)
    if got is None:
        raise ProbeError(f"acolyte {finder} never received its ration")

    p = settle(port, lambda s: s.is_checked(SUB_FOOD))
    check("water AND a ration on the same acolyte checks the food subobjective",
          p.is_checked(SUB_FOOD), str(p))
    check("both subobjectives satisfied completes 'Prepare an expedition'",
          p.is_completed(OBJ_EXPEDITION), str(p))
    # A fully-satisfied composite leaves the default active view (#958's
    # hide rule), taking its subobjective rows with it. Pinned here
    # because it is the ONE place the panel's rows and the durable state
    # legitimately disagree: the completion is permanent, the rows are not.
    check("the satisfied composite drops out of the active view",
          (p.row(OBJ_EXPEDITION) or {}).get("active") is False, str(p))
    check("its subobjective rows go with it",
          SUB_WATER not in p.row_ids and SUB_FOOD not in p.row_ids, str(p.row_ids))


def phase_latch(port: int, finder: int) -> None:
    """Removing the supplies afterwards must not untick the completion."""
    strip_supplies(port, finder)
    litres, rations = carried(port, finder)
    check("the supplies are gone again",
          litres == 0.0 and rations == 0, f"{litres} L, {rations} rations")

    p = settle(port, lambda s: not s.is_checked(SUB_WATER))
    check("removing the water unchecks the live water subobjective",
          p.is_checked(SUB_WATER) is False, str(p))
    check("removing the ration unchecks the live food subobjective",
          p.is_checked(SUB_FOOD) is False, str(p))
    check("the completed 'Prepare an expedition' objective stays completed",
          p.is_completed(OBJ_EXPEDITION), str(p))
    check("the earlier full objectives stay completed too",
          p.is_completed(OBJ_PORTAL) and p.is_completed(OBJ_WATER), str(p))


def phase_save(port: int) -> list[str]:
    before = progress(port).completed
    accepted = send(port, f"return engine.saveWorld('{PAGE}', '{SLOT}')", timeout=30.0)
    if accepted != "true":
        raise ProbeError(f"engine.saveWorld was not accepted: {accepted!r}")
    req = capture_request_id(port, "return engine.getSaveStatus()")
    ok, status = wait_save_complete(port, req) if req is not None else (False, None)
    check("the save completes through the real save barrier", ok, str(status))
    return before


def phase_reload(port: int, expected: list[str]) -> None:
    accepted = send(port, f"return engine.loadSave('{SLOT}')", timeout=30.0)
    if accepted != "true":
        raise ProbeError(f"engine.loadSave was not accepted: {accepted!r}")
    req = capture_request_id(port, "return engine.getLoadStatus()")
    published, status = wait_load_published(port, request_id=req)
    if not check("the save loads and publishes in a fresh process",
                 published, str(status)):
        return

    p = settle(port, lambda s: s.is_completed(OBJ_EXPEDITION), seconds=30.0)
    check("every completed full objective survives the round trip",
          sorted(p.completed) == sorted(expected),
          f"{sorted(p.completed)} != {sorted(expected)}")
    open_state = hud_open(port)
    check("the HUD comes back collapsed after a load",
          open_state == "false", open_state)
    check("the live subobjectives recompute from the LOADED world "
          "(no supplies were saved, so both read unchecked)",
          p.is_checked(SUB_WATER) is False and p.is_checked(SUB_FOOD) is False,
          str(p))
    check("the completed composite is active again with its unchecked rows",
          (p.row(OBJ_EXPEDITION) or {}).get("active") is True
          and SUB_WATER in p.row_ids and SUB_FOOD in p.row_ids, str(p))


# --------------------------------------------------------------------------
# #996: a branch that latches BEFORE it is ever revealed
#
# A separate boot and a separate acolyte on purpose: by the time the main
# flow above reaches "Prepare an expedition", "Secure water source" has
# already completed, so the composite is always revealed while it is still
# incomplete there -- exactly the case the module's OWN hide-on-completion
# rule already covered. Reproducing the bug needs the opposite order: the
# unstripped spawn kit satisfies both prepare subobjectives (and so latches
# the composite) before secure_water_source ever completes, since nothing
# about carrying supplies has anything to do with discovering water.
# --------------------------------------------------------------------------
def phase_pre_latched_baseline(port: int) -> None:
    # tutorial_eval's own tick is what checks the subobjectives and
    # latches the composite -- it is not pause-gated, but it still needs
    # at least one tick after the AI state just materialized, so this
    # settles rather than reading a single snapshot.
    p = settle(port, lambda s: s.is_completed(OBJ_EXPEDITION))
    check("the unstripped acolyte's spawn kit checks both prepare "
          "subobjectives immediately", p.is_checked(SUB_WATER)
          and p.is_checked(SUB_FOOD), str(p))
    check("the composite latches before it is ever revealed",
          p.is_completed(OBJ_EXPEDITION), str(p))
    check("the composite is not yet observable -- neither the portal nor "
          "the water objective has completed", OBJ_EXPEDITION not in p.row_ids,
          str(p.row_ids))


def phase_pre_latched_portal(port: int, gx: int, gy: int) -> int:
    valid = send(port, f"local v = building.canPlaceAt('{PORTAL_DEF}', {gx}, {gy}); "
                       f"return tostring(v)", timeout=20.0)
    if valid != "true":
        raise ProbeError(f"the camp tile ({gx},{gy}) will not take a portal: {valid}")
    raw = send(port, f"return tostring(building.spawn('{PORTAL_DEF}', {gx}, {gy}))",
               timeout=20.0)
    if raw in ("nil", "", "false"):
        raise ProbeError(f"building.spawn('{PORTAL_DEF}') failed at ({gx},{gy})")
    bid = int(float(raw))
    # No roster this time: the probe's own acolyte is the only one whose
    # supplies matter, and a spawned roster would be provisioned too.
    send(port, f"return tostring(building.setSpawnRemaining({bid}, 0))", timeout=15.0)

    p = settle(port, lambda s: s.is_completed(OBJ_PORTAL))
    check("placing the portal completes the portal objective",
          p.is_completed(OBJ_PORTAL), str(p))
    check("the already-latched composite still stays hidden behind "
          "'Secure water source', which has not completed yet",
          OBJ_EXPEDITION not in p.row_ids and p.is_completed(OBJ_WATER) is False,
          str(p))
    return bid


def phase_pre_latched_reveal(port: int, uid: int, sx: int, sy: int) -> None:
    """The acolyte discovers real water -- secure_water_source completes,
    and the already-latched prepare branch is revealed for the FIRST
    time. The checklist must stay non-empty (#996's whole point)."""
    send(port, f"unit.setPos({uid}, {sx}, {sy}); return 'ok'", timeout=15.0)
    set_paused(port, False)
    found = poll_until(60.0, lambda: known_water_count(port, uid) > 0)
    set_paused(port, True)
    if found is None:
        raise ProbeError(f"acolyte {uid} never discovered the generated water")

    p = settle(port, lambda s: s.is_completed(OBJ_WATER))
    check("discovering water completes 'Secure water source'",
          p.is_completed(OBJ_WATER), str(p))
    # place_portal/secure_water still leave the default checklist view
    # (they were revealed while still incomplete, so the ordinary
    # hide-on-completion rule applies to them unchanged); only the
    # already-latched prepare branch is the new, sticky exception.
    check("the already-latched prepare branch is observable in authored "
          "preorder, not an empty checklist (#996)",
          p.active_row_ids == [OBJ_EXPEDITION, SUB_WATER, SUB_FOOD],
          str(p.active_row_ids))
    check("place_portal and secure_water are retained as completed "
          "history, not re-shown in the active view",
          OBJ_PORTAL in p.row_ids and OBJ_WATER in p.row_ids
          and OBJ_PORTAL not in p.active_row_ids
          and OBJ_WATER not in p.active_row_ids, str(p))
    row = p.row(OBJ_EXPEDITION) or {}
    check("the composite renders active, with its normal completed marker",
          row.get("active") is True and row.get("completed") is True, str(row))
    check("both prepare subobjectives render active and checked",
          p.is_checked(SUB_WATER) and p.is_checked(SUB_FOOD)
          and (p.row(SUB_WATER) or {}).get("active") is True
          and (p.row(SUB_FOOD) or {}).get("active") is True, str(p))


def phase_pre_latched_reversal(port: int, uid: int) -> None:
    """Removing the supplies afterwards must still uncheck the live
    subobjectives, keep the branch observable, and never touch the
    durable completion (the same reversal contract check 6 already
    covers, now proven for a branch that started out sticky)."""
    strip_supplies(port, uid)
    p = settle(port, lambda s: not s.is_checked(SUB_WATER))
    check("removing the water unchecks the live water subobjective",
          p.is_checked(SUB_WATER) is False, str(p))
    check("removing the ration unchecks the live food subobjective",
          p.is_checked(SUB_FOOD) is False, str(p))
    check("the prepare branch stays in the ACTIVE checklist rather than "
          "vanishing (it never hides once already-latched-sticky)",
          OBJ_EXPEDITION in p.active_row_ids and SUB_WATER in p.active_row_ids
          and SUB_FOOD in p.active_row_ids, str(p.active_row_ids))
    check("the composite's durable completion is untouched",
          p.is_completed(OBJ_EXPEDITION), str(p))


# --------------------------------------------------------------------------
# Save-slot hygiene
# --------------------------------------------------------------------------
def remove_probe_slot() -> None:
    """Delete only this probe's own save slot, under the repo's saves/."""
    saves = os.path.join(REPO_ROOT, "saves")
    target = os.path.join(saves, SLOT)
    if (os.path.basename(target) == SLOT
            and os.path.dirname(target) == saves
            and os.path.isdir(target)
            and not os.path.islink(target)):
        shutil.rmtree(target)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9424)
    args = ap.parse_args()

    remove_probe_slot()
    started = time.time()

    print(f"== boot 1: build the session (port {args.port}) ==")
    proc = boot(args.port, log=LOG_A, label="tutorial engine")
    completed: list[str] = []
    try:
        load_content(args.port)
        generate_world(args.port, args.seed, args.size)
        load_scripts(args.port)
        set_paused(args.port, True)

        wx, wy = find_water_tile(args.port)
        sx, sy = find_shore_tile(args.port, wx, wy)
        cx, cy = find_camp_tile(args.port, wx, wy)
        print(f"   generated water at ({wx},{wy}); shore ({sx},{sy}); "
              f"camp ({cx},{cy})")

        print("== 1. a fresh session shows only the first objective ==")
        phase_baseline(args.port)

        print("== 2. place the acolyte portal ==")
        phase_portal(args.port, cx, cy)

        print("== 3. a two-acolyte party, stripped of its spawn kit ==")
        finder, mate = phase_party(args.port, cx, cy)

        print("== 4. discover real water, then share it over the radio ==")
        phase_discover_and_share(args.port, finder, mate, sx, sy)

        print("== 5. restore the supplies one step at a time ==")
        phase_supplies(args.port, finder)

        print("== 6. removing the supplies must not untick the completion ==")
        phase_latch(args.port, finder)

        print("== 7. save ==")
        completed = phase_save(args.port)
    except ProbeError as e:
        print(f"\nSETUP FAILED: {e}")
        failures.append(f"setup: {e}")
        return 1
    finally:
        quit_engine(args.port, proc)

    print(f"== boot 2: load the save in a FRESH process (port {args.port}) ==")
    proc = boot(args.port, log=LOG_B, label="tutorial reload engine")
    try:
        load_content(args.port)
        load_scripts(args.port)
        print("== 8. the round trip ==")
        phase_reload(args.port, completed)
    except ProbeError as e:
        print(f"\nRELOAD FAILED: {e}")
        failures.append(f"reload: {e}")
    finally:
        quit_engine(args.port, proc)
        remove_probe_slot()

    print(f"== boot 3: a branch that latches before it is ever revealed "
          f"(#996, port {args.port}) ==")
    proc = boot(args.port, log=LOG_C, label="tutorial pre-latch engine")
    try:
        load_content(args.port)
        generate_world(args.port, args.seed, args.size)
        load_scripts(args.port)
        set_paused(args.port, True)

        wx, wy = find_water_tile(args.port)
        sx, sy = find_shore_tile(args.port, wx, wy)
        cx, cy = find_camp_tile(args.port, wx, wy)

        print("== 9. an unstripped acolyte latches the composite before "
              "it is ever revealed ==")
        uid = spawn_player_acolyte(args.port, cx + 2, cy)
        got = poll_until(20.0, lambda: carried(args.port, uid)[1] > 0)
        if got is None:
            raise ProbeError(f"acolyte {uid} never materialized its spawn kit")
        set_paused(args.port, False)
        got = poll_until(30.0, lambda: known_water_count(args.port, uid) >= 0)
        set_paused(args.port, True)
        if got is None:
            raise ProbeError("the spawned acolyte never received AI state")
        phase_pre_latched_baseline(args.port)

        print("== 10. placing the portal keeps the branch hidden behind "
              "'Secure water source' ==")
        phase_pre_latched_portal(args.port, cx, cy)

        print("== 11. discovering water reveals the branch already "
              "complete ==")
        phase_pre_latched_reveal(args.port, uid, sx, sy)

        print("== 12. removing supplies unchecks live subobjectives, "
              "keeps the branch observable ==")
        phase_pre_latched_reversal(args.port, uid)
    except ProbeError as e:
        print(f"\nPRE-LATCHED-BRANCH LEG FAILED: {e}")
        failures.append(f"pre-latched branch: {e}")
    finally:
        quit_engine(args.port, proc)

    print(f"\n({time.time() - started:.0f}s)")
    if failures:
        print(f"FAILED ({len(failures)}):")
        for f in failures:
            print(f"  - {f}")
        return 1
    print("ALL CHECKS PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
