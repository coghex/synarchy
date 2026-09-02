#!/usr/bin/env python3
"""Unit tests for action_outcome_probe.py's fixture discovery.

Its chop-fixture stage (#1398), and its tillable-box discovery's two
`world.getFluidAt` reads (#1793).

ENGINE-FREE, and deliberately so: `action_outcome_probe.py` boots a real
headless engine and generates a 64-world, which is the ~8-minute cost
this coverage exists to stop depending on. Nothing here boots an engine,
opens a TCP console, or generates a world — the probe's own `send` and
`send_json` are swapped for a fake console that answers the exact Lua
strings the real stage sends. Same model as `test_run_probes.py` (real
code paths, synthetic inputs) and `test_persistence_contract_sweep.py`
(pure, <1 s).

The real `tools/action_outcome_probe.py` is imported and driven, so this
exercises the shipped `find_chop_fixture` / `evaluate_chop_designation` /
`run_chop_stage` / `run_and_report_chop_stage` / `probe_exit_status`
paths rather than a copy of them.

Covered:
  * a covered search that finds no wood-bearing flora produces the
    FIXTURE-SETUP diagnostic and exit 2;
  * a discovered wood coordinate followed by an invalid chop record
    produces the BEHAVIOR diagnostic and exit 1 — one case per way a
    record can be invalid (absent, wrong kind, not partial, miscounted,
    nothing applied, nothing dropped, malformed counts);
  * a well-formed record passes and exits 0;
  * exit 2 takes precedence over a concurrent ordinary failure, and an
    ordinary failure (a missing till box, an unusable portal fixture)
    never becomes exit 2;
  * discovery really uses the authoritative query, builds the real
    `chop.designateInstances` request from the returned coordinate's own
    plant identity, and rejects a malformed one;
  * the query origins COVER the probe's loaded region, which is the
    property that makes "found nothing" a statement about the region
    rather than about a sample grid.

And, for `find_mixed_box` (#1793):
  * its anchor filter rejects a flat, flora-free WET candidate and
    scans on, while the same lattice with nothing wet returns that
    first candidate;
  * a WET neighbour marks the 5x5 box mixed, exactly as a sloped or
    flora-bearing one already does, and a uniform box is not mixed;
  * both paths collapse `world.getFluidAt`'s multi-return before
    transport, so neither can decode the tab-joined text as a table.

Injecting a hand-constructed invalid record here does not conflict with
the issue's ban on synthetic outcome records: that ban is scoped to the
real-engine check, where the pass must come from a real designation and
a real destructive drain. This test asserts on the FAILURE branches,
which a real engine cannot be made to produce on demand.

Usage:
  python3 tools/test_action_outcome_probe.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import io
import math
import re
import sys
from contextlib import redirect_stdout
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

import action_outcome_probe as probe  # noqa: E402
# The arity-boundary fake console (#1793) lives in its sibling
# self-test, so the two files cannot drift on what the debug console
# really returns for a multi-return query.
from test_tillable_fluid_filter import (  # noqa: E402
    Tile, TerrainConsole)

FAILURES: list[str] = []

PORT = 0  # never used — the fake console ignores it


def check(name, condition, detail=""):
    if condition:
        print(f"  [PASS] {name}")
    else:
        FAILURES.append(f"{name}{(': ' + detail) if detail else ''}")
        print(f"  [FAIL] {name}{(': ' + detail) if detail else ''}")


class FakeConsole:
    """Answers the exact Lua strings the chop stage sends.

    `wood_at` is the coordinate `world.findHarvestableFlora` reports (or
    None for "nothing in range anywhere"), `drain` the value the drain
    following the designation returns. `instance_id` is what the
    follow-up `world.getFloraAt` on that coordinate reports as the
    discovered tree's identity — #1856 designates by exact id, so the
    stage reads one. Every call is recorded so a test can assert WHICH
    query was used and what it was centred on."""

    def __init__(self, wood_at=None, species="oak", drain=None,
                 wood_result=None, instance_id=4242, flora_result=None):
        self.wood_at = wood_at
        self.species = species
        self.drain = drain if drain is not None else []
        self.wood_result = wood_result  # overrides a well-formed reply
        self.instance_id = instance_id
        self.flora_result = flora_result  # overrides the getFloraAt reply
        self.sent: list[str] = []
        self._designated = False

    # -- the two entry points action_outcome_probe uses ---------------
    def send(self, port, lua, timeout=10.0):
        self.sent.append(lua)
        if "chop.designate" in lua:
            self._designated = True
        return '"ok"'

    def send_json(self, port, lua, timeout=10.0, idle=None):
        self.sent.append(lua)
        if "getFloraAt" in lua:
            if self.flora_result is not None:
                return self.flora_result
            return {"id": self.species, "instanceId": self.instance_id,
                    "chopDesignated": False, "harvestable": True,
                    "regrowthRemaining": 0, "tags": ["wood"]}
        if "findHarvestableFlora" in lua:
            if self.wood_result is not None:
                return self.wood_result
            if self.wood_at is None:
                return {}          # the engine returns nil -> empty table
            gx, gy = self.wood_at
            return {"gx": gx, "gy": gy, "id": self.species, "dist": 0.0}
        if "drainActionOutcomes" in lua:
            # The clear-noise drain before the designation is empty; the
            # one after it carries the record under test.
            return self.drain if self._designated else []
        raise AssertionError(f"unexpected console call: {lua}")

    def designate_calls(self):
        return [c for c in self.sent if "chop.designateInstances" in c]

    def find_calls(self):
        return [c for c in self.sent if "findHarvestableFlora" in c]


def drive(console, fn=None):
    """Run the chop stage against `console`, capturing its output."""
    fn = fn or probe.run_and_report_chop_stage
    original_send, original_json = probe.send, probe.send_json
    probe.send, probe.send_json = console.send, console.send_json
    buffer = io.StringIO()
    try:
        with redirect_stdout(buffer):
            result = fn(PORT)
    finally:
        probe.send, probe.send_json = original_send, original_json
    return result, buffer.getvalue()


def good_record(requested=2, applied=1, dropped=1, outcome="partial",
                kind="chop.designate"):
    return [{"kind": kind, "outcome": outcome, "requested": requested,
             "applied": applied, "dropped": dropped,
             "where": {"x": 0.0, "y": 0.0}, "ts": 1.0}]


# ---------------------------------------------------------------------
# Setup classification: no authoritative wood result -> diagnostic + 2
# ---------------------------------------------------------------------
def test_no_wood_anywhere_is_a_setup_failure():
    console = FakeConsole(wood_at=None)
    (ok, setup_failed), out = drive(console)
    check("no wood anywhere classifies as fixture setup",
          setup_failed is True and ok is False, f"{ok=} {setup_failed=}")
    check("the setup diagnostic names the fixture, not the contract",
          "(fixture setup)" in out and "UNVERIFIED" in out, out.strip())
    check("the setup diagnostic exits 2",
          probe.probe_exit_status(ok, setup_failed) == 2)
    check("a setup failure never designates anything",
          console.designate_calls() == [],
          str(console.designate_calls()))
    check("a setup failure searched every covering origin",
          len(console.find_calls()) == len(probe.chop_search_origins()) ** 2,
          f"{len(console.find_calls())} calls")


def test_malformed_wood_result_is_a_setup_failure():
    for label, reply in [
        ("missing gy", {"gx": 10, "id": "oak"}),
        ("non-numeric gx", {"gx": "ten", "gy": 10, "id": "oak"}),
        ("boolean gx", {"gx": True, "gy": 10, "id": "oak"}),
        ("not a table", "nil"),
    ]:
        console = FakeConsole(wood_result=reply)
        (ok, setup_failed), out = drive(console)
        check(f"a malformed wood result ({label}) is a setup failure",
              setup_failed is True and console.designate_calls() == [],
              f"{setup_failed=} {console.designate_calls()=}")


def test_setup_failure_outranks_an_ordinary_failure():
    check("exit 2 takes precedence over a concurrent ordinary failure",
          probe.probe_exit_status(False, True) == 2)
    check("exit 2 applies even when everything else passed",
          probe.probe_exit_status(True, True) == 2)


def test_ordinary_failures_never_become_exit_2():
    # A missing till box and an unusable portal fixture both set
    # `passed = False` without touching the chop flag (#1398's
    # correction: exit 2 is reserved for CHOP-fixture discovery alone).
    check("an ordinary failure exits 1, not 2",
          probe.probe_exit_status(False, False) == 1)
    check("a clean run exits 0",
          probe.probe_exit_status(True, False) == 0)


# ---------------------------------------------------------------------
# Behavior classification: a discovered coordinate + a bad record -> 1
# ---------------------------------------------------------------------
def test_invalid_records_are_behavior_failures():
    cases = {
        "absent record": [],
        "empty drain object": {},
        "wrong kind": good_record(kind="till.designate"),
        "not partial": good_record(outcome="accepted", dropped=0,
                                   requested=25, applied=25),
        "requested is not 25": good_record(requested=9, applied=1,
                                           dropped=8),
        "requested != applied + dropped": good_record(requested=25,
                                                      applied=1, dropped=23),
        "nothing applied": good_record(requested=25, applied=0, dropped=25),
        "nothing dropped": good_record(requested=25, applied=25, dropped=0),
        "malformed counts": good_record(requested="25", applied=1,
                                        dropped=24),
    }
    for label, drain in cases.items():
        console = FakeConsole(wood_at=(12, -34), drain=drain)
        (ok, setup_failed), out = drive(console)
        check(f"an invalid chop record ({label}) is a behavior failure",
              ok is False and setup_failed is False,
              f"{ok=} {setup_failed=}")
        check(f"an invalid chop record ({label}) exits 1, not 2",
              probe.probe_exit_status(ok, setup_failed) == 1)
        check(f"an invalid chop record ({label}) reports the chop line, "
              f"not the setup diagnostic",
              "(fixture setup)" not in out and "mixed chop selection" in out,
              out.strip())


def test_a_well_formed_record_passes():
    console = FakeConsole(wood_at=(12, -34), drain=good_record())
    (ok, setup_failed), out = drive(console)
    check("a well-formed partial record passes",
          ok is True and setup_failed is False, f"{ok=} {setup_failed=}")
    check("a passing run exits 0",
          probe.probe_exit_status(ok, setup_failed) == 0)
    check("the pass line names the coordinate and species",
          "(12,-34)" in out and "oak" in out, out.strip())


# ---------------------------------------------------------------------
# Discovery uses the authoritative query, centred on its own answer
# ---------------------------------------------------------------------
def test_discovery_uses_the_authoritative_wood_query():
    console = FakeConsole(wood_at=(12, -34), drain=good_record())
    drive(console)
    first = console.find_calls()[0]
    check("discovery calls world.findHarvestableFlora with the wood tag",
          "world.findHarvestableFlora" in first and "'wood'" in first, first)
    # #1856: the point query is no longer a SEARCH fallback — it is
    # asked exactly once, afterwards, and only about the coordinate
    # discovery already returned. A search that fell back to it would
    # ask about coordinates the authoritative query never named.
    flora_calls = [c for c in console.sent if "getFloraAt" in c]
    check("the point query is never used to SEARCH",
          len(flora_calls) == 1 and "(12,-34)" in flora_calls[0],
          str(flora_calls))
    check("discovery stops at the first hit",
          len(console.find_calls()) == 1, str(console.find_calls()))


def test_designation_is_the_real_public_exact_id_request():
    console = FakeConsole(wood_at=(12, -34), instance_id=4242,
                          drain=good_record())
    drive(console)
    calls = console.designate_calls()
    check("exactly one chop.designateInstances is issued",
          len(calls) == 1, str(calls))
    # The submitted set is the DISCOVERED tree plus one well-formed id
    # naming no resident plant, which is what makes the partial leg
    # deterministic rather than a hope about nearby grass.
    check("the request submits the discovered tree's own instance id",
          "4242" in calls[0], calls[0])
    check("the request submits one unresolvable id alongside it",
          str(probe.UNRESOLVABLE_INSTANCE_ID) in calls[0], calls[0])
    check("the unresolvable id is in the planted namespace and positive",
          probe.UNRESOLVABLE_INSTANCE_ID >> 62 == 1
          and probe.UNRESOLVABLE_INSTANCE_ID < (1 << 63),
          str(probe.UNRESOLVABLE_INSTANCE_ID))
    check("the request names the probe page and the wood tag",
          "'probe'" in calls[0] and "'wood'" in calls[0], calls[0])


def test_a_missing_instance_id_is_a_behavior_failure():
    """A discovered coordinate whose point query reports no identity
    cannot be designated at all. That is a BEHAVIOR failure, not a
    fixture-setup one: discovery already proved wood is there."""
    console = FakeConsole(wood_at=(12, -34), drain=good_record(),
                          flora_result={"id": "oak"})
    (ok, setup_failed), out = drive(console)
    check("a discovered tile with no instance id fails as behavior",
          ok is False and setup_failed is False, f"{ok=} {setup_failed=}")
    check("and nothing is designated",
          not console.designate_calls(), str(console.designate_calls()))


def test_the_designation_drain_is_destructive_and_isolated():
    console = FakeConsole(wood_at=(12, -34), drain=good_record())
    drive(console)
    # Index by POSITION, not by value: both drains send the identical
    # Lua string, so list.index would report the first one twice.
    drains = [i for i, c in enumerate(console.sent)
              if "drainActionOutcomes" in c]
    designate_at = next(i for i, c in enumerate(console.sent)
                        if "chop.designate" in c)
    check("the stage clears the ring before designating",
          any(i < designate_at for i in drains), str(drains))
    check("the stage drains again after designating",
          any(i > designate_at for i in drains), str(drains))


# ---------------------------------------------------------------------
# The covering-origins property
# ---------------------------------------------------------------------
def test_search_origins_cover_the_loaded_region():
    origins = probe.chop_search_origins()
    lo, hi, radius = (probe.LOADED_TILE_MIN, probe.LOADED_TILE_MAX,
                      probe.FIND_RADIUS)
    check("the region's endpoints are themselves origins",
          origins[0] == lo and origins[-1] == hi, str(origins))
    check("origins are strictly ascending",
          all(b > a for a, b in zip(origins, origins[1:])), str(origins))
    # Worst case is the point furthest from every origin: the centre of
    # the widest cell, on both axes at once. findHarvestableFlora measures
    # EUCLIDEAN distance, so that corner-most point must still fall inside
    # one radius-64 disc.
    widest = max(b - a for a, b in zip(origins, origins[1:]))
    worst = math.hypot(widest / 2, widest / 2)
    check("every tile in the loaded region lies inside some origin's disc",
          worst <= radius, f"worst-case distance {worst:.2f} > {radius}")
    # And a brute-force check over the real region, not just the bound.
    step = 7  # coprime with the 64 spacing, so it lands off-lattice
    uncovered = [
        (x, y)
        for x in range(lo, hi + 1, step)
        for y in range(lo, hi + 1, step)
        if not any(math.hypot(x - ox, y - oy) <= radius
                   for ox in origins for oy in origins)
    ]
    check("a swept sample of the loaded region is fully covered",
          not uncovered, f"{len(uncovered)} uncovered, e.g. {uncovered[:3]}")


def test_the_loaded_region_matches_what_the_probe_loads():
    """The coverage argument is only sound while these constants really
    describe the region main() loads — so read them back off the source
    rather than trusting the comment."""
    source = Path(probe.__file__).read_text()
    match = re.search(r"world\.loadChunksInRegion\((-?\d+), *(-?\d+), *"
                      r"(-?\d+), *(-?\d+)\)", source)
    check("main() still loads a chunk region the constants describe",
          match is not None
          and int(match.group(1)) == probe.LOADED_CHUNK_MIN
          and int(match.group(3)) == probe.LOADED_CHUNK_MAX,
          match.group(0) if match else "no loadChunksInRegion found")


# ---------------------------------------------------------------------
# find_mixed_box's two fluid reads (#1793)
# ---------------------------------------------------------------------
# The scan's own lattice: range(-span*8, span*8+1, 3) at the declared
# span=6, so the first two anchors it considers are these.
FIRST_ANCHOR = (-48, -48)
SECOND_ANCHOR = (-48, -45)


def drive_mixed_box(console, **kwargs):
    """Run the shipped find_mixed_box against a TerrainConsole."""
    original_send, original_json = probe.send, probe.send_json
    probe.send, probe.send_json = console.send, console.send_json
    try:
        return probe.find_mixed_box(PORT, **kwargs)
    finally:
        probe.send, probe.send_json = original_send, original_json


def neighbours(anchor):
    """The 5x5 box find_mixed_box sweeps around `anchor`, centre aside."""
    ax, ay = anchor
    return [(ax + dx, ay + dy)
            for dx in range(-2, 3) for dy in range(-2, 3)
            if (dx, dy) != (0, 0)]


def test_a_wet_anchor_is_rejected():
    # The first anchor is under water; the second has a wet neighbour, so
    # it is the box the scan should settle on.
    console = TerrainConsole({
        FIRST_ANCHOR: Tile(fluid="lake"),
        neighbours(SECOND_ANCHOR)[0]: Tile(fluid="river"),
    })
    found = drive_mixed_box(console)
    check("find_mixed_box rejects a flat, flora-free WET anchor",
          found == SECOND_ANCHOR, f"returned {found}, wanted {SECOND_ANCHOR}")
    check("find_mixed_box really queried the wet anchor",
          console.queried("getFluidAt", *FIRST_ANCHOR),
          "the wet anchor was never asked about")


def test_a_wet_neighbour_marks_the_box_mixed():
    cases = [("a wet neighbour", Tile(fluid="river")),
             ("a sloped neighbour", Tile(slope=1)),
             ("a flora-bearing neighbour", Tile(flora={"id": "oak"}))]
    for label, tile in cases:
        console = TerrainConsole({neighbours(FIRST_ANCHOR)[0]: tile})
        found = drive_mixed_box(console)
        check(f"{label} marks the 5x5 box mixed",
              found == FIRST_ANCHOR,
              f"returned {found}, wanted {FIRST_ANCHOR}")


def test_a_uniform_region_is_never_mixed():
    console = TerrainConsole()
    found = drive_mixed_box(console)
    check("a wholly flat, dry, flora-free region yields no mixed box",
          found is None, f"returned {found}")


def test_both_fluid_reads_collapse_the_multi_return():
    console = TerrainConsole({
        FIRST_ANCHOR: Tile(fluid="lake"),
        neighbours(SECOND_ANCHOR)[0]: Tile(fluid="river"),
    })
    drive_mixed_box(console)
    check("find_mixed_box asks about fluid on both the anchor and its box",
          console.queried("getFluidAt", *SECOND_ANCHOR)
          and console.queried("getFluidAt", *neighbours(SECOND_ANCHOR)[0]),
          "one of the two fluid reads never ran")
    check("neither fluid read leaves the multi-return uncaptured",
          console.uncaptured_fluid_calls() == [],
          str(console.uncaptured_fluid_calls()[:1]))


def main():
    print("action_outcome_probe fixture-discovery tests (engine-free)\n")
    test_no_wood_anywhere_is_a_setup_failure()
    test_malformed_wood_result_is_a_setup_failure()
    test_setup_failure_outranks_an_ordinary_failure()
    test_ordinary_failures_never_become_exit_2()
    test_invalid_records_are_behavior_failures()
    test_a_well_formed_record_passes()
    test_discovery_uses_the_authoritative_wood_query()
    test_designation_is_the_real_public_exact_id_request()
    test_a_missing_instance_id_is_a_behavior_failure()
    test_the_designation_drain_is_destructive_and_isolated()
    test_search_origins_cover_the_loaded_region()
    test_the_loaded_region_matches_what_the_probe_loads()
    test_a_wet_anchor_is_rejected()
    test_a_wet_neighbour_marks_the_box_mixed()
    test_a_uniform_region_is_never_mixed()
    test_both_fluid_reads_collapse_the_multi_return()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return 1
    print("\nAll action_outcome_probe fixture-discovery tests passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
