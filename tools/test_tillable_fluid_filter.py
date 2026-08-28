#!/usr/bin/env python3
"""Unit tests for the tillable-tile fluid filter in three probes (#1793).

ENGINE-FREE, and deliberately so: `till_probe.py`, `plant_probe.py` and
`farm_ai_probe.py` each boot a real headless engine and generate a
world, which is the multi-minute cost this coverage exists to stop
depending on. Nothing here boots an engine, opens a TCP console, or
generates a world — each probe's own `send_json` is swapped for a fake
console that answers the exact Lua strings the real scan sends. Same
model as `test_action_outcome_probe.py` (#1398): the shipped functions
are imported and driven, not a copy of them.

The defect (#1793): `world.getFluidAt` is a MULTI-RETURN query whose
ARITY is the contract (`Engine.Scripting.Lua.API.WorldQuery.Fluid`) — a
fluid tile pushes the type string AND the fluid surface z, a dry tile
pushes a single nil, and it never pushes a table. Five sites across four
probes decoded it as a JSON object, so
`isinstance(fluid, dict) and fluid.get("type")` was never true and every
one of those fluid filters was dead: `find_tillable` could return a
coordinate under water. This file owns three of those sites;
`test_action_outcome_probe.py` owns the other two.

`TerrainConsole` below is the single definition of that arity boundary
and `test_action_outcome_probe.py` imports it, so the two files cannot
drift on what the console really returns. It answers a fluid query the
way the real pipeline does, and it answers by ARITY:

  * a wet tile queried through the collapsed form
    (``local t = world.getFluidAt(x,y); return t``) replies with the one
    type string, which reaches Python as the ``str`` ``river``;
  * a wet tile queried through the UNCAPTURED form
    (``return world.getFluidAt(x,y)``) replies with BOTH values, which
    `Engine.Scripting.Lua.Thread.Console` joins with a tab and
    `probelib.send_json` hands back as the un-JSON text ``river"\t12``.

A fake that answered a clean object to either form would conceal exactly
the failure this issue repairs, so it answers neither.

Decoding is the SHIPPED decoder: the console text is rendered the way
`luaValueToText` renders it, put through `probelib.send`'s documented
quote-strip, and handed to the real `probelib.send_json`.

Covered, for `till_probe.find_tillable`, `plant_probe.find_tillable` and
`farm_ai_probe.find_tillable`:
  * a flat, flora-free WET candidate is rejected and the scan continues
    to the next, DRY lattice point;
  * with nothing wet anywhere the same scan returns that first candidate,
    so the rejection above is the fluid filter and not the scan stalling;
  * the wet candidate really was queried, so it was not skipped by some
    other filter;
  * every fluid query collapses the multi-return BEFORE transport;
  * the slope and flora filters still reject their own tiles, and the
    flora filter still reads a table (`getFloraAt` really is table-or-nil,
    `Engine.Scripting.Lua.API.Forage.Query`), unchanged by this issue;
  * the sampling lattice each function walks is unchanged.

Usage:
  python3 tools/test_tillable_fluid_filter.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import json
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

import probelib  # noqa: E402

import farm_ai_probe  # noqa: E402
import plant_probe  # noqa: E402
import till_probe  # noqa: E402

FAILURES: list[str] = []

PORT = 0  # never used — the fake console ignores it

# The fluid surface z the engine pushes alongside a wet tile's type.
FLUID_SURFACE_Z = 12

_POINT_QUERY = re.compile(
    r"world\.(getSlopeAt|getFluidAt|getFloraAt)"
    r"\(\s*(-?\d+)\s*,\s*(-?\d+)\s*\)")
# The uncaptured form: the query's results go straight into `return`, so
# the console sees every one of them.
_UNCAPTURED_FLUID = re.compile(r"return\s+world\.getFluidAt\b")


def check(name, condition, detail=""):
    if condition:
        print(f"  [PASS] {name}")
    else:
        FAILURES.append(f"{name}{(': ' + detail) if detail else ''}")
        print(f"  [FAIL] {name}{(': ' + detail) if detail else ''}")


# ---------------------------------------------------------------------
# The console side, rendered the way the engine renders it
# ---------------------------------------------------------------------
def render_console_reply(values):
    """Render Lua return values as `Console.hs` puts them on the wire.

    `luaValueToText` emits `null` for nil, a QUOTED string for a string,
    a bare numeral for a number and JSON for a table; the console then
    joins every returned value with a tab
    (`Engine.Scripting.Lua.Thread.Console`)."""
    parts = []
    for value in values:
        if value is None:
            parts.append("null")
        elif isinstance(value, bool):
            parts.append("true" if value else "false")
        elif isinstance(value, str):
            parts.append('"' + value + '"')
        elif isinstance(value, (int, float)):
            parts.append(repr(value) if isinstance(value, float)
                         else str(value))
        else:
            parts.append(json.dumps(value, separators=(",", ":")))
    return "\t".join(parts) if parts else "ok"


def decode_reply(console_text):
    """Put `console_text` through the SHIPPED probe-side decoder.

    `probelib.send` strips the surrounding quotes off a result line, and
    the real `probelib.send_json` decodes what is left — non-JSON text
    unchanged. Stubbing `send` rather than reimplementing `send_json`
    keeps the decode under test the one the probes actually run."""
    stripped = console_text.strip('"')

    def fake_send(port, lua, timeout=10.0, idle=None, expect_result=True):
        return stripped

    original = probelib.send
    probelib.send = fake_send
    try:
        return probelib.send_json(PORT, "")
    finally:
        probelib.send = original


class Tile:
    """One point's terrain. Defaults are flat, dry and flora-free."""

    def __init__(self, slope=0, fluid=None, flora=None):
        self.slope = slope
        self.fluid = fluid      # a fluid type string, or None for dry
        self.flora = flora      # a dict the engine would push as a table


FLAT_DRY = Tile()


class TerrainConsole:
    """Answers the three point queries a tillable scan sends.

    `tiles` maps (gx, gy) -> `Tile`; any point not named is flat, dry and
    flora-free. Every call is recorded so a test can assert WHICH query
    was sent and, for fluid, whether the multi-return was collapsed
    before transport."""

    def __init__(self, tiles=None):
        self.tiles = dict(tiles or {})
        self.sent: list[str] = []

    def tile(self, gx, gy):
        return self.tiles.get((gx, gy), FLAT_DRY)

    def send(self, port, lua, timeout=10.0, expect_result=True, idle=None):
        self.sent.append(lua)
        return "ok"

    def send_json(self, port, lua, timeout=10.0, idle=None):
        self.sent.append(lua)
        match = _POINT_QUERY.search(lua)
        if not match:
            raise AssertionError(f"unexpected console call: {lua}")
        query = match.group(1)
        gx, gy = int(match.group(2)), int(match.group(3))
        tile = self.tile(gx, gy)
        if query == "getSlopeAt":
            values = [tile.slope]
        elif query == "getFloraAt":
            values = [tile.flora]
        else:
            if tile.fluid is None:
                # Dry, or an unloaded chunk: ONE nil either way, so both
                # query shapes see the same single value.
                values = [None]
            elif _UNCAPTURED_FLUID.search(lua):
                # The caller never reduced the query: the console sees
                # both results and tab-joins them.
                values = [tile.fluid, FLUID_SURFACE_Z]
            else:
                values = [tile.fluid]
        return decode_reply(render_console_reply(values))

    # -- call inspection ----------------------------------------------
    def calls(self, query):
        return [lua for lua in self.sent if f"world.{query}(" in lua]

    def queried(self, query, gx, gy):
        return any(_POINT_QUERY.search(lua)
                   and _POINT_QUERY.search(lua).groups()
                   == (query, str(gx), str(gy))
                   for lua in self.sent)

    def uncaptured_fluid_calls(self):
        return [lua for lua in self.calls("getFluidAt")
                if _UNCAPTURED_FLUID.search(lua)]


def drive(module, console, call):
    """Run `call` with `module`'s console entry points swapped out."""
    original_send, original_json = module.send, module.send_json
    module.send, module.send_json = console.send, console.send_json
    try:
        return call()
    finally:
        module.send, module.send_json = original_send, original_json


# ---------------------------------------------------------------------
# The three finders, driven through their own declared defaults
# ---------------------------------------------------------------------
# Each scan walks its own lattice; these are the first two points of it,
# read off the shipped ranges. A test makes the FIRST one wet and expects
# the SECOND — so a scan that widened or reordered its lattice fails here
# rather than passing vacuously.
FINDERS = [
    ("till_probe", till_probe, (-64, -64), (-64, -60)),
    ("plant_probe", plant_probe, (-64, -64), (-64, -60)),
    ("farm_ai_probe", farm_ai_probe, (-64, -64), (-64, -60)),
]


def test_a_wet_candidate_is_rejected_for_the_next_dry_one():
    for name, module, wet, dry in FINDERS:
        console = TerrainConsole({wet: Tile(fluid="river")})
        found = drive(module, console, lambda: module.find_tillable(PORT))
        check(f"{name}.find_tillable skips a flat, flora-free WET tile",
              found == dry, f"returned {found}, wanted {dry}")
        check(f"{name}.find_tillable really queried the wet candidate",
              console.queried("getFluidAt", *wet),
              "the wet tile was never asked about")
        check(f"{name}.find_tillable collapses the multi-return before "
              f"transport",
              console.uncaptured_fluid_calls() == [],
              str(console.uncaptured_fluid_calls()[:1]))


def test_the_same_scan_returns_the_first_candidate_when_nothing_is_wet():
    for name, module, wet, _dry in FINDERS:
        console = TerrainConsole()
        found = drive(module, console, lambda: module.find_tillable(PORT))
        check(f"{name}.find_tillable returns the first candidate when the "
              f"whole lattice is dry",
              found == wet, f"returned {found}, wanted {wet}")


def test_a_wet_tile_never_decodes_as_a_table():
    """The predicate this issue replaced tested `isinstance(fluid, dict)`.
    Pin why that could never fire, for BOTH query shapes."""
    console = TerrainConsole({(0, 0): Tile(fluid="river")})
    collapsed = console.send_json(
        PORT, "local t = world.getFluidAt(0,0); return t")
    uncaptured = console.send_json(PORT, "return world.getFluidAt(0,0)")
    check("a collapsed wet reply decodes as the bare type string",
          collapsed == "river", repr(collapsed))
    check("an uncaptured wet reply decodes as tab-joined text, not a table",
          isinstance(uncaptured, str) and uncaptured.startswith("river")
          and "\t" in uncaptured, repr(uncaptured))
    check("neither wet reply is ever a dict",
          not isinstance(collapsed, dict) and not isinstance(uncaptured, dict),
          f"{type(collapsed)} {type(uncaptured)}")
    dry = console.send_json(PORT, "local t = world.getFluidAt(9,9); return t")
    check("a dry reply decodes to None through either shape",
          dry is None
          and console.send_json(PORT, "return world.getFluidAt(9,9)") is None,
          repr(dry))


def test_the_slope_and_flora_filters_are_unchanged():
    for name, module, wet, dry in FINDERS:
        sloped = TerrainConsole({wet: Tile(slope=1)})
        found = drive(module, sloped, lambda: module.find_tillable(PORT))
        check(f"{name}.find_tillable still rejects a sloped tile",
              found == dry, f"returned {found}, wanted {dry}")
        flora = TerrainConsole({wet: Tile(flora={"id": "oak"})})
        found = drive(module, flora, lambda: module.find_tillable(PORT))
        check(f"{name}.find_tillable still rejects a flora-bearing tile",
              found == dry, f"returned {found}, wanted {dry}")
        check(f"{name}.find_tillable still reads flora as a table",
              flora.queried("getFloraAt", *wet),
              "the flora query never ran")


def test_the_sampling_lattices_are_unchanged():
    """Requirement 7: this issue touches the fluid predicate and nothing
    about WHERE each probe looks. Read the walked points back off a full
    dry sweep, which visits every one of them."""
    for name, module, _first, _second in FINDERS:
        # Nothing is tillable anywhere, so the scan runs to exhaustion.
        exhausted = TerrainConsole(
            {(x, y): Tile(slope=1)
             for x in range(-96, 97) for y in range(-96, 97)})
        drive(module, exhausted, lambda: module.find_tillable(PORT))
        points = sorted({(int(m.group(2)), int(m.group(3)))
                         for m in (_POINT_QUERY.search(lua)
                                   for lua in exhausted.calls("getSlopeAt"))
                         if m})
        expected = sorted((x, y)
                          for x in range(-64, 65, 4)
                          for y in range(-64, 65, 4))
        check(f"{name}.find_tillable still walks its declared lattice",
              points == expected,
              f"{len(points)} points, first {points[:1]}, last {points[-1:]}")


def main():
    print("tillable-tile fluid filter tests (engine-free)\n")
    test_a_wet_candidate_is_rejected_for_the_next_dry_one()
    test_the_same_scan_returns_the_first_candidate_when_nothing_is_wet()
    test_a_wet_tile_never_decodes_as_a_table()
    test_the_slope_and_flora_filters_are_unchanged()
    test_the_sampling_lattices_are_unchanged()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return 1
    print("\nAll tillable-tile fluid filter tests passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
