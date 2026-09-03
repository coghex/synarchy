#!/usr/bin/env python3
"""The natural-water, camp and party harness shared by both scenario
owners of `tools/tutorial_probe.py` (#2145).

Everything the probe does to REAL generated geography and REAL units,
in one place: finding a generated lake or river and a water-free,
portal-eligible camp far from it; placing the camp's portal without its
automatic roster; spawning player-faction acolytes and shedding or
restoring their spawn kit; reading what a unit carries and what water it
knows; pinning the clock to noon; and the facing/freeze/teleport
instruments plus the `SightSnapshot` evidence that makes the sight proof
conclusive rather than circumstantial.

Both stage owners consume this module and neither duplicates any of it —
the sight, inventory and geography helpers exist exactly once, which is
what keeps the two scenarios asserting the same instrument.

The portal placement lives here rather than in a stage owner for the
same reason: it is the camp fixture's own step, and both legs place the
identical roster-suppressed portal.

This module BOOTS NOTHING; the facade owns every engine lifecycle call.
"""
from __future__ import annotations

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import send, poll_until
from tutorial_probe_contracts import (CANTEEN_DEF, EXPEDITION_WATER_L, PAGE,
                                      PORTAL_DEF, RATIONS_DEF, ProbeError)
from tutorial_probe_setup import set_paused

# How far the party camps from any water. Comfortably past the acolyte
# FOV scan radius, so a camped acolyte cannot discover the lake before
# the probe teleports it there.
CAMP_CLEARANCE = 12
CAMP_MIN_DISTANCE = 20

# The hour every sight-based check in this probe runs at, and the sun
# angle that hour MUST read back as once the engine has applied it.
# World.Time.Types.worldTimeToSunAngle is (h * 60 + m) / 1440, and
# World.Time.Local.localSunAngle adds a u = gx - gy longitude offset, so
# the global angle is the one read at the origin: 720 / 1440 = 0.5.
PIN_HOUR = 12
PIN_MINUTE = 0
PIN_SUN_ANGLE = 0.5
PIN_SUN_TOLERANCE = 0.01

#: Compass name for each single-tile step, spelled the way
#: `unit.setFacing` parses it (Engine.Scripting.Lua.API.Units.Selection)
#: and oriented the way Unit.LineOfSight's `facingVector` is: +y is
#: south.
STEP_DIRECTION = {
    (0, -1): "N",  (1, -1): "NE", (1, 0): "E",  (1, 1): "SE",
    (0, 1): "S",   (-1, 1): "SW", (-1, 0): "W", (-1, -1): "NW",
}


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


def find_fixture_site(port: int) -> tuple[tuple[int, int], tuple[int, int],
                                          tuple[int, int]]:
    """The three tiles every scenario is staged on: the generated water,
    the dry shore beside it, and the far camp. Returned together so both
    legs select them the same way, in the same order."""
    wx, wy = find_water_tile(port)
    sx, sy = find_shore_tile(port, wx, wy)
    cx, cy = find_camp_tile(port, wx, wy)
    return (wx, wy), (sx, sy), (cx, cy)


# --------------------------------------------------------------------------
# The camp's portal
# --------------------------------------------------------------------------
def place_portal_without_roster(port: int, gx: int, gy: int) -> int:
    """Place the acolyte portal at the camp and suppress its automatic
    starting roster.

    The suppression is why this is shared rather than per-scenario: both
    legs supply their OWN acolytes so they control the supply state, and
    every acolyte spawns with a full canteen and two rations
    (data/units/acolyte.yaml starting_inventory) that would satisfy both
    prepare subobjectives the instant a roster appeared.
    """
    valid = send(port, f"local v = building.canPlaceAt('{PORTAL_DEF}', {gx}, {gy}); "
                       f"return tostring(v)", timeout=20.0)
    if valid != "true":
        raise ProbeError(f"the camp tile ({gx},{gy}) will not take a portal: {valid}")
    raw = send(port, f"return tostring(building.spawn('{PORTAL_DEF}', {gx}, {gy}))",
               timeout=20.0)
    if raw in ("nil", "", "false"):
        raise ProbeError(f"building.spawn('{PORTAL_DEF}') failed at ({gx},{gy})")
    bid = int(float(raw))
    send(port, f"return tostring(building.setSpawnRemaining({bid}, 0))", timeout=15.0)
    return bid


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


def wait_for_spawn_kit(port: int, uid: int) -> None:
    """Inventories materialize asynchronously on the unit thread; wait for
    the spawn kit to actually exist before anything reads or sheds it, or
    a strip runs against an empty inventory and silently does nothing."""
    if poll_until(20.0, lambda: carried(port, uid)[1] > 0) is None:
        raise ProbeError(f"acolyte {uid} never materialized its spawn kit")


def grant_ai_state(port: int, uids: list[int], failure: str) -> None:
    """One short UNPAUSED window, just long enough for
    scripts/unit_ai.lua to create AI state for every `uid`.

    Spawning happens paused, so the evaluator cannot see the new units at
    all until they have state — which is what lets the spawn kit be shed
    before it is ever evaluated. `known_water_count` returns -1 while
    there is no state, so `>= 0` is the "state exists" predicate.
    """
    set_paused(port, False)
    got = poll_until(30.0, lambda: all(known_water_count(port, u) >= 0
                                       for u in uids))
    set_paused(port, True)
    if got is None:
        raise ProbeError(failure)


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


def refill_canteen(port: int, uid: int) -> None:
    """Put the expedition's worth of water back into the shed canteen."""
    send(port, f"return unit.modifyItemFill({uid}, '{CANTEEN_DEF}', "
               f"{EXPEDITION_WATER_L})", timeout=15.0)


def give_ration(port: int, uid: int, failure: str) -> None:
    """Hand one ration back and wait for the unit thread to apply it."""
    send(port, f"return tostring(unit.addItem({uid}, '{RATIONS_DEF}', 0))",
         timeout=15.0)
    if poll_until(20.0, lambda: carried(port, uid)[1] > 0) is None:
        raise ProbeError(failure)


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


def sun_angle_at(port: int, gx: int, gy: int) -> float | None:
    """`world.getSunAngleAt` — the longitude-local day/night phase (0..1)
    at a tile, or None when the engine cannot answer it (no active
    world, or a malformed reply)."""
    raw = send(port, f"local a = world.getSunAngleAt({gx}, {gy}); "
                     f"return a == nil and 'nil' or tostring(a)", timeout=15.0)
    try:
        return float(raw)
    except (TypeError, ValueError):
        return None


def pin_daylight(port: int, page: str = PAGE) -> float:
    """Pin `page`'s clock to local noon and WAIT until the engine has
    applied it, so every sight-based check in this probe is
    deterministic. Returns the settled global sun angle.

    #1230 made `unit.getVisibleTiles` scale its radius by the page-local
    `nightPerceptionFactor`: at midnight a perception-1.0 unit sees 3
    tiles where it sees 6 at noon. Without this, whether the tutorial's
    water objective is reachable would depend on what hour the run
    happened to reach — the exact flakiness a probe must not have. Noon
    is the maximum, so pinning it here keeps the objective's difficulty
    at what it was before the night factor applied to the binary set.

    Two things this deliberately does not shortcut, both found by #1771:

      * `world.setTime` takes (pageId, hour, minute)
        (Engine.Scripting.Lua.API.World.Clock). The two-argument call
        this helper used to make matched no branch of that decoder, so
        it queued NOTHING — every caller then ran at whatever hour
        worldgen happened to leave behind while believing it was noon.
      * Even spelled correctly the call only ENQUEUES a WorldSetTime for
        the world thread. It becomes observable through
        `world.getSunAngleAt`, which World.Thread.Time mirrors from the
        VISIBLE page's own clock on every tick — including while paused,
        because that mirror sits outside the pause gate. Polling it is
        what makes "pinned" a fact rather than a hope, and it is how
        tools/circadian_probe.py waits for the same command.
    """
    send(port, f"world.setTime('{page}', {PIN_HOUR}, {PIN_MINUTE}); return 'ok'",
         timeout=15.0)
    seen: list[float | None] = []

    def settled() -> bool:
        angle = sun_angle_at(port, 0, 0)
        seen.append(angle)
        return angle is not None and abs(angle - PIN_SUN_ANGLE) <= PIN_SUN_TOLERANCE

    if poll_until(20.0, settled) is None:
        raise ProbeError(
            f"world.setTime('{page}', {PIN_HOUR}, {PIN_MINUTE}) never took "
            f"effect: world.getSunAngleAt(0, 0) last read "
            f"{seen[-1] if seen else 'nothing'!r}, not "
            f"{PIN_SUN_ANGLE} +/- {PIN_SUN_TOLERANCE}")
    return seen[-1]


def face_toward(port: int, uid: int, fx: int, fy: int,
                tx: int, ty: int) -> str:
    """Turn `uid`, standing at (fx, fy), to look at the adjacent tile
    (tx, ty). Returns the compass name applied.

    `unit.getVisibleTiles` is cone-limited — Unit.LineOfSight's `inCone`
    is a 120° wedge centred on the unit's facing — so a tile directly
    BESIDE a unit is genuinely invisible to it when it is looking the
    other way. A teleport does not touch facing
    (Unit.Thread.Command.Lifecycle), and the acolyte's facing coming out
    of the unpaused AI-state window is wherever it last happened to
    walk. Leaving that to chance would make "can it see the water"
    depend on an accident of the AI's wander, which is the same class of
    flakiness as depending on the hour — so the probe sets it, through
    the engine's own synchronous setter, before asserting what the
    engine reports. Nothing about sight, the cone, or the objective
    changes; only the setup stops being random.

    CALL THIS ON A FROZEN UNIT. `unit.setFacing` writes the render-facing
    instance the FOV query reads, and Unit.Thread.publishToRender
    overwrites that from the sim state's own `usFacing` on its very next
    tick — a tick that runs whether or not the engine is paused. Skipping
    that overwrite for a frozen unit is exactly what the flag is for
    ("so Lua's setAnim / setFacing / setPos aren't stomped"); without the
    freeze this setter reliably lasts less than a tenth of a second.
    """
    dx, dy = tx - fx, ty - fy
    step = ((dx > 0) - (dx < 0), (dy > 0) - (dy < 0))
    name = STEP_DIRECTION.get(step)
    if name is None:
        raise ProbeError(f"cannot face unit {uid} at ({fx},{fy}) toward "
                         f"({tx},{ty}): they are the same tile")
    ok = send(port, f"return tostring(unit.setFacing({uid}, '{name}'))",
              timeout=15.0)
    if ok != "true":
        raise ProbeError(f"unit.setFacing({uid}, '{name}') failed: {ok!r}")
    return name


def set_frozen(port: int, uid: int, on: bool, required: bool = True) -> None:
    """`unit.setFrozen` — pin (or release) the render-facing instance a
    unit's sight is read from.

    Unit.Thread.publishToRender republishes position, facing and pose
    from the sim state every tick unless the instance is frozen. Freezing
    is therefore what makes a probe-set position or facing SURVIVE, and
    what keeps the field of view this probe asserted the same field of
    view the AI scans for the whole window. It stops nothing else: the
    Lua AI still ticks a frozen unit, and
    scripts/unit_ai_water.lua's `scanForWater` still runs at 10 Hz off
    `unit.getVisibleTiles` — which is precisely how the main discovery
    path can freeze its recipient and still watch a radio share land in
    its memory.

    `required` is False on the RELEASE calls: they run on the way out of
    a failure, where the unit may already be the thing that went wrong,
    and a second exception raised there would bury the diagnostic that
    explains the first.
    """
    got = send(port, f"return tostring(unit.setFrozen({uid}, "
                     f"{'true' if on else 'false'}))", timeout=15.0)
    if required and got != "true":
        raise ProbeError(f"unit.setFrozen({uid}, {on}) failed: {got!r}")


def wait_for_teleport(port: int, uid: int, gx: int, gy: int,
                      page: str = PAGE, timeout: float = 20.0) -> bool:
    """Poll until `unit.setPos` has actually landed `uid` on
    (page, gx, gy).

    `unit.setPos` only QUEUES a UnitTeleport for the unit thread
    (Engine.Scripting.Lua.API.Units.Spawn) and returns before it is
    applied, so a caller that reads sight straight afterwards can be
    reading the unit's OLD tile. The unit thread drains its command
    queue OUTSIDE the pause gate (Unit.Thread.unitTick), which is what
    makes this readback work in the probe's paused setup window.
    """
    expected = f"{page},{gx},{gy}"

    def landed() -> bool:
        raw = send(port,
                   f"local i = unit.getInfo({uid}); "
                   f"if not i then return 'missing' end; "
                   f"return tostring(i.page) .. ',' .. math.floor(i.gridX) "
                   f".. ',' .. math.floor(i.gridY)", timeout=15.0)
        return raw == expected

    return poll_until(timeout, landed) is not None


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


class SightSnapshot:
    """One classifying observation of a unit's sight conditions (#1771).

    Everything `Unit.LineOfSight.visibleTilesOnPage` derives its answer
    from — the unit's page, position, facing and the night-scaled clock
    — read in ONE console call together with the question that actually
    matters: did that exact target tile come back in the FOV set? A
    reveal that does not happen is then classifiable instead of a bare
    uid: an unapplied teleport, a wrong page, a facing that excluded the
    pond and an AI that walked away all read differently, and "the unit
    could see it and the discovery did not fire" reads differently again.
    Page and position are BOTH rendered as requested-vs-observed pairs,
    because a unit that reached the right coordinates on the wrong page
    is looking at a different page's tiles and is not a sight failure at
    all.

    Every field is either a value or an EXPLICIT absence marker
    (``missing`` / ``unavailable`` / ``unknown``). A vanished unit, an AI
    module that never produced state and an unanswerable clock each
    render as themselves rather than raising some unrelated parse or
    nil-access error inside the very diagnostic that exists to explain a
    failure.
    """

    def __init__(self, label: str, uid: int, raw: str,
                 requested: tuple[int, int], target: tuple[int, int],
                 requested_page: str = PAGE) -> None:
        self.label = label
        self.uid = uid
        self.raw = raw
        self.requested = requested
        self.requested_page = requested_page
        self.target = target
        self.fields: dict[str, str] = {}
        for part in raw.split(";"):
            key, sep, value = part.partition("=")
            if sep:
                self.fields[key.strip()] = value

    def get(self, key: str, default: str = "unknown") -> str:
        value = self.fields.get(key)
        return default if value in (None, "", "nil") else value

    @property
    def unit_present(self) -> bool:
        return self.fields.get("unit") == "present"

    @property
    def observed(self) -> tuple[int, int] | None:
        try:
            return int(self.fields["gx"]), int(self.fields["gy"])
        except (KeyError, ValueError):
            return None

    @property
    def position_applied(self) -> bool:
        return self.observed == self.requested

    @property
    def page_applied(self) -> bool:
        return self.fields.get("page") == self.requested_page

    @property
    def teleport_applied(self) -> bool:
        """Both halves of what `unit.setPos` was asked for. A unit that
        reached the right COORDINATES on the wrong page is not where the
        caller sent it, and its field of view is a different page's."""
        return self.page_applied and self.position_applied

    @property
    def sees_target(self) -> bool | None:
        """True / False, or None when the engine could not answer."""
        answer = self.fields.get("target")
        if answer == "yes":
            return True
        if answer == "no":
            return False
        return None

    def classify(self) -> str:
        """The one sentence a reader needs: WHY this observation looks
        the way it does, in the order the causes have to be ruled out."""
        if not self.unit_present:
            return (f"unit {self.uid} does not exist — unit.getInfo returned "
                    f"nil, so nothing about its sight can be read")
        if not self.teleport_applied:
            return (f"the requested teleport to {self.requested} on page "
                    f"{self.requested_page} was NOT applied — the unit is at "
                    f"{self.observed} on page {self.get('page')}")
        if self.sees_target is None:
            return ("the engine could not report a field of view for the unit "
                    f"(unit.getVisibleTiles: {self.get('fov')}), so its sight "
                    "of the target cannot be classified")
        if self.sees_target:
            return (f"the unit COULD see the target water tile {self.target} "
                    f"— sight is not the explanation")
        return (f"the unit could NOT see the target water tile {self.target} "
                f"(facing {self.get('facing')}, {self.get('fovCount')} tiles "
                f"in view, {self.get('water')} of them water)")

    def detail_lines(self) -> list[str]:
        return [
            f"observed page={self.get('page')} "
            f"pos=({self.get('gx')},{self.get('gy')}) z={self.get('gz')} "
            f"facing={self.get('facing')} | requested page="
            f"{self.requested_page} pos=({self.requested[0]},"
            f"{self.requested[1]}) "
            f"[{'applied' if self.teleport_applied else 'NOT applied'}]",
            f"target water ({self.target[0]},{self.target[1]}) in field of "
            f"view: {self.get('target')} | unit.getVisibleTiles: "
            f"{self.get('fov')}, {self.get('fovCount')} tiles, "
            f"{self.get('water')} of them water",
            f"sun angle: global {self.get('sunGlobal')}, local at the unit "
            f"{self.get('sunLocal')} | pinned {PIN_HOUR:02d}:{PIN_MINUTE:02d} "
            f"= {PIN_SUN_ANGLE:.4f}",
            f"AI state: {self.get('ai')} | currentAction="
            f"{self.get('action')} activeGoal={self.get('goal')} "
            f"knownWaterSources={self.get('known')}",
        ]

    def render(self) -> str:
        return "\n".join([f"  [{self.label}] {self.classify()}"]
                         + [f"      {line}" for line in self.detail_lines()])

    def __str__(self) -> str:
        return self.render()


def sight_snapshot(port: int, uid: int, label: str,
                   requested: tuple[int, int], target: tuple[int, int],
                   requested_page: str = PAGE) -> SightSnapshot:
    """Capture one `SightSnapshot` in a single console round trip.

    One call, because the point is a COHERENT picture: position, facing,
    FOV and clock read across four separate round trips could each
    describe a different instant, which is exactly the ambiguity the
    snapshot exists to remove.
    """
    wx, wy = target
    lua = (
        "local out = {}; "
        "local function put(k, v) out[#out+1] = k .. '=' .. tostring(v) end; "
        "local function num(v) return v == nil and 'unknown' "
        "or string.format('%.4f', v) end; "
        f"local i = unit.getInfo({uid}); "
        "if not i then put('unit', 'missing') else "
        "put('unit', 'present'); put('page', i.page); "
        "put('gx', math.floor(i.gridX)); put('gy', math.floor(i.gridY)); "
        "put('gz', i.gridZ); put('facing', i.facing) end; "
        "put('sunGlobal', num(world.getSunAngleAt(0, 0))); "
        "put('sunLocal', i and num(world.getSunAngleAt(math.floor(i.gridX), "
        "math.floor(i.gridY))) or 'unknown'); "
        f"local tiles = unit.getVisibleTiles({uid}); "
        "if not tiles then put('fov', 'unavailable'); "
        "put('fovCount', 'unknown'); put('target', 'unknown'); "
        "put('water', 'unknown') else local seen, water = false, 0; "
        "for _, t in ipairs(tiles) do "
        f"if math.floor(t.x) == {wx} and math.floor(t.y) == {wy} "
        "then seen = true end; local f = world.getFluidAt(t.x, t.y); "
        "if f == 'lake' or f == 'river' then water = water + 1 end end; "
        "put('fov', 'ok'); put('fovCount', #tiles); "
        "put('target', seen and 'yes' or 'no'); put('water', water) end; "
        "local ok, ai = pcall(require, 'scripts.unit_ai'); "
        f"local s = (ok and ai and ai.getState) and ai.getState({uid}) or nil; "
        "if not s then put('ai', 'missing'); put('action', 'unknown'); "
        "put('goal', 'unknown'); put('known', 'unknown') else "
        "put('ai', 'present'); put('action', s.currentAction); "
        "put('goal', s.activeGoal); "
        "put('known', #(s.knownWaterSources or {})) end; "
        "return table.concat(out, ';')"
    )
    return SightSnapshot(label, uid, send(port, lua, timeout=30.0),
                         requested, target, requested_page)


def sight_failure(headline: str, *snapshots: SightSnapshot) -> str:
    """A ProbeError message that CLASSIFIES a sight failure rather than
    naming a uid — #1771 requirement 4. Every snapshot handed in is
    preserved, so a pre-poll picture and a failure-time picture sit side
    by side and an unapplied teleport reads differently from an AI that
    walked away afterwards."""
    return "\n".join([headline] + [s.render() for s in snapshots])
