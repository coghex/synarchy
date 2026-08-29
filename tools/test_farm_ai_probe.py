#!/usr/bin/env python3
"""Unit tests for the farm probe's harvest-yield oracle (issue #1760).

ENGINE-FREE, and deliberately so: `tools/farm_ai_probe.py` boots a real
headless engine and generates a world, so its phase-9 capstone is only
ever exercised by an eleven-minute manual run. Nothing here boots an
engine, opens a TCP console or generates a world.

The defect (#1760): phase 9 scored the auto-harvest by catching
`wheat_grain` lying on the ground at ONE instant. That instant is an
intermediate of the very action it waits for — `world.harvestFlora`
spawns the yield and clears the plot, and
`scripts/unit_ai_harvest.lua`'s `collecting` phase then pulls it in one
item per tick — so the check broke its wait on the first phase's own
effect and then sampled a ground the second phase was emptying. Since
#1743 that collecting phase is reachable under ordinary arbitration, so
the emptying reliably completes and the old oracle failed precisely on
the successful runs.

`YieldTrail` replaces the instant with an observation trail, and this
file pins its classification oracle deterministically. `ingest` is split
from the console read exactly so these four outcomes can be driven with
no engine:

  * produced-on-ground — confirmed at the plot tile and still there;
  * produced-carried — the same `instanceId` followed into the worker's
    inventory, whether or not it was first caught on the ground;
  * moved-after-pickup — confirmed at the tile, then confirmed gone,
    never seen carried (the collecting phase or a hungry acolyte);
  * not-produced — the ONLY failing outcome.

Also pinned, because each is a way the oracle would silently stop
meaning what it says:

  * the baseline armed BEFORE the time scale goes up excludes grain that
    already existed, on the ground and in the inventory alike;
  * a `nil` ground answer whose page did NOT resolve is not a
    disappearance — the second return of `item.getGroundForUnit` is
    load-bearing (`Engine.Scripting.Lua.API.Items.Ground`);
  * an empty Lua table decodes as the JSON object `{}`, never `[]`, so a
    quiet sample must not read as a decode failure;
  * a gid the unit AI recorded but the owning page never confirmed at
    the plot tile is DIAGNOSTIC, never production evidence;
  * the query resolves every candidate through the worker-owning-page
    `item.getGroundForUnit` (#1666) and scopes to the plot's own tile,
    so the active-page `item.listGround()` can only nominate candidates
    and `clear_wild_forage`'s sweep yields can never satisfy the check;
  * a failure's diagnostics name the worker's action, the recorded gids
    and every place the yield was looked for (#1760 requirement 4).

Usage:
  python3 tools/test_farm_ai_probe.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import sys
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
import farm_ai_probe as probe  # type: ignore  # noqa: E402

PLOT_X, PLOT_Y = 12, -4
DEF = probe.HARVEST_YIELD_DEF

FAILURES: list[str] = []


def check(name: str, condition: bool, detail: str = "") -> None:
    if condition:
        print(f"  [PASS] {name}")
    else:
        FAILURES.append(name)
        print(f"  [FAIL] {name}{(': ' + detail) if detail else ''}")


def trail() -> "probe.YieldTrail":
    return probe.YieldTrail(PLOT_X, PLOT_Y)


def sample(loot=(), ground=(), gone=(), inv=(), phase="", action=""):
    """One decoded console sample, in the shape the engine's own
    serializer produces: an EMPTY Lua table reaches Python as `{}`
    (`Engine.Scripting.Lua.API.Shell.luaTableToJson`), never `[]`."""
    def field(seq):
        return list(seq) if seq else {}
    return {
        "phase": phase,
        "action": action,
        "loot": field(loot),
        "ground": field(ground),
        "gone": field(gone),
        "inv": field(inv),
    }


def ground_row(gid, instance, x=None, y=None):
    return {
        "gid": gid,
        "instanceId": instance,
        "x": PLOT_X + 0.42 if x is None else x,
        "y": PLOT_Y + 0.61 if y is None else y,
    }


# ---------------------------------------------------------------------
# The four classifications
# ---------------------------------------------------------------------

def test_not_produced() -> None:
    t = trail()
    t.arm_from(sample())
    for _ in range(4):
        t.ingest(sample(phase="", action="wander"))
    check("a run that produced nothing classifies not-produced",
          t.classify() == probe.YIELD_NOT_PRODUCED, t.report())
    check("not-produced is the failing outcome", not t.produced(), t.report())


def test_produced_on_ground() -> None:
    t = trail()
    t.arm_from(sample())
    t.ingest(sample(loot=[7], ground=[ground_row(7, 4001)],
                    phase="collecting", action="auto_harvest"))
    check("a yield confirmed at the plot tile classifies on-ground",
          t.classify() == probe.YIELD_ON_GROUND, t.report())
    check("on-ground is a passing outcome", t.produced(), t.report())
    check("on-ground names the gid it found",
          "gid(s) 7" in t.report(), t.report())


def test_produced_carried_after_ground() -> None:
    t = trail()
    t.arm_from(sample())
    t.ingest(sample(loot=[7], ground=[ground_row(7, 4001)],
                    phase="collecting", action="auto_harvest"))
    t.ingest(sample(gone=[7], inv=[4001], action="auto_harvest"))
    check("a yield followed into the inventory classifies carried",
          t.classify() == probe.YIELD_CARRIED, t.report())
    check("carried is a passing outcome", t.produced(), t.report())
    check("the carried instance is linked to its ground observation",
          t.carried[4001]["linked"] is True, str(t.carried))
    check("the report names both places the yield was seen",
          "left the ground" in t.report() and "carried" in t.report(),
          t.report())


def test_produced_carried_without_ground_observation() -> None:
    """The collection can outrun the poll cadence entirely. A
    `wheat_grain` instance that did not exist when the trail was armed
    was still produced by the plot: `data/flora/crops.yaml` is the only
    declaration yielding it and its wild worldgen density is 0."""
    t = trail()
    t.arm_from(sample())
    t.ingest(sample(inv=[4002], action="auto_harvest"))
    check("a carried instance with no ground sighting still passes",
          t.classify() == probe.YIELD_CARRIED and t.produced(), t.report())
    check("an unlinked carried instance is reported as unlinked",
          t.carried[4002]["linked"] is False, str(t.carried))


def test_moved_after_pickup() -> None:
    t = trail()
    t.arm_from(sample())
    t.ingest(sample(loot=[9], ground=[ground_row(9, 4100)]))
    t.ingest(sample(gone=[9]))
    check("a yield that left the ground unseen classifies "
          "moved-after-pickup",
          t.classify() == probe.YIELD_MOVED, t.report())
    check("moved-after-pickup is a passing outcome", t.produced(), t.report())


# ---------------------------------------------------------------------
# The rules that keep those classifications honest
# ---------------------------------------------------------------------

def test_arming_excludes_preexisting_grain() -> None:
    t = trail()
    t.arm_from(sample(ground=[ground_row(3, 3999)], inv=[3900]))
    t.ingest(sample(ground=[ground_row(3, 3999)], inv=[3900]))
    check("grain already on the ground when armed is never credited",
          t.classify() == probe.YIELD_NOT_PRODUCED, t.report())
    check("grain already carried when armed is never credited",
          not t.carried, str(t.carried))
    t.ingest(sample(ground=[ground_row(3, 3999), ground_row(4, 4200)]))
    check("a NEW gid after arming is still credited",
          t.classify() == probe.YIELD_ON_GROUND and set(t.ground) == {4},
          t.report())


def test_unresolved_page_is_not_a_disappearance() -> None:
    """`item.getGroundForUnit` returns `nil, false` when the unit has no
    live page — nothing was determined. The query only ever lists a gid
    under `gone` when the page DID resolve, so a sample that lists
    neither must leave the trail where it was."""
    t = trail()
    t.arm_from(sample())
    t.ingest(sample(ground=[ground_row(11, 4300)]))
    t.ingest(sample())
    check("a sample determining nothing leaves the yield on the ground",
          t.classify() == probe.YIELD_ON_GROUND, t.report())
    check("no gid was retired by a sample that reported none",
          not t.gone(), str(t.gone()))


def test_empty_tables_decode_without_error() -> None:
    t = trail()
    t.arm_from(sample())
    check("an all-empty sample arms an empty baseline",
          t.armed and not t.armed_gids and not t.armed_instances)
    t.ingest({"phase": "", "action": "", "loot": {}, "ground": {},
              "gone": {}, "inv": {}})
    check("an all-empty sample leaves the trail not-produced",
          t.classify() == probe.YIELD_NOT_PRODUCED, t.report())
    check("_rows reads an empty Lua object as an empty list",
          probe._rows({}) == [] and probe._rows([1, 2]) == [1, 2])
    check("_ids ignores non-numeric and boolean entries",
          probe._ids([1, "x", True, 2.0, None]) == [1, 2])


def test_recorded_gid_alone_is_not_production_evidence() -> None:
    """`s.harvestLoot` says the AI harvested SOMETHING; it does not say
    which plant. Only the owning-page confirmation at the plot tile —
    or a carried instance — may carry the check."""
    t = trail()
    t.arm_from(sample())
    t.ingest(sample(loot=[21, 22], phase="collecting"))
    t.ingest(sample(gone=[21, 22]))
    check("an AI-recorded gid the page never confirmed does not pass",
          t.classify() == probe.YIELD_NOT_PRODUCED, t.report())
    check("that gid is still recorded for diagnostics",
          t.loot_gids == [21, 22], str(t.loot_gids))


def test_regression_the_old_oracle_shape() -> None:
    """The exact assessed run that #1760 was filed for: the plot cleared,
    farming XP grew, and no grain was on the ground when it was looked
    for. The old check scored that as two failures."""
    t = trail()
    t.arm_from(sample())
    t.ingest(sample(loot=[31], ground=[ground_row(31, 4400)],
                    phase="collecting", action="auto_harvest"))
    t.ingest(sample(gone=[31], inv=[4400], action="auto_harvest"))
    ripe_sampled = False
    plot_cleared = True
    xp_grew = True
    ok9a = ripe_sampled or (plot_cleared and (t.produced() or xp_grew))
    ok9 = plot_cleared and t.produced()
    check("a completely successful autonomous harvest is not scored as "
          "a failure", ok9a and ok9,
          f"ok9a={ok9a} ok9={ok9} trail={t.report()}")


# ---------------------------------------------------------------------
# The console query's own contracts
# ---------------------------------------------------------------------

def test_query_uses_the_owning_page_contract() -> None:
    q = trail().query(77, known=(5, 3))
    check("every candidate is resolved through item.getGroundForUnit",
          "item.getGroundForUnit(77,g)" in q, q)
    check("the active-page listing only nominates candidates",
          "item.listGround()" in q and "add(r.id)" in q
          and "o.ground[#o.ground+1]={gid=r.id" not in q, q)
    check("a ground row is built from the owning-page entry",
          "o.ground[#o.ground+1]={gid=g,instanceId=e.instanceId" in q, q)
    check("gone is recorded only when the page resolved",
          "elseif (not e) and p then o.gone[#o.gone+1]=g end" in q, q)
    check("known gids are carried back into the candidate set",
          "for _,g in ipairs({3,5}) do add(g) end" in q, q)


def test_query_scopes_to_the_plot_tile_and_def() -> None:
    q = trail().query(77)
    check("the owning-page entry is tile-scoped to the plot",
          f"math.floor(e.x)=={PLOT_X} and math.floor(e.y)=={PLOT_Y}" in q, q)
    check("the discovery scan is tile-scoped to the plot",
          f"math.floor(r.x)=={PLOT_X} and math.floor(r.y)=={PLOT_Y}" in q, q)
    check("both scans filter on the yield's def name",
          q.count(f"defName=='{DEF}'") == 3, q)
    check("the worker's carried instances are read by instanceId",
          "unit.getInventory(77)" in q
          and "o.inv[#o.inv+1]=r.instanceId" in q, q)
    check("the unit AI's own harvest record is sampled",
          "ai.getState(77)" in q and "s.harvestLoot" in q
          and "s.harvestPhase" in q and "s.currentAction" in q, q)
    check("the query is a single console line",
          "\n" not in q, repr(q[:80]))


# ---------------------------------------------------------------------
# Failure diagnostics (#1760 requirement 4)
# ---------------------------------------------------------------------

def test_diagnostics_distinguish_the_two_failures() -> None:
    moved = trail()
    moved.arm_from(sample())
    moved.ingest(sample(loot=[41], ground=[ground_row(41, 4500)],
                        phase="collecting", action="auto_harvest"))
    moved.ingest(sample(gone=[41], inv=[4500], action="idle"))
    text = moved.diagnostics()
    check("diagnostics name the worker's last action",
          "currentAction='idle'" in text, text)
    check("diagnostics name the recorded harvest gids",
          "world.harvestFlora: [41]" in text, text)
    check("diagnostics name where the yield was found",
          "instanceId 4500 carried by the worker" in text
          and "confirmed on the ground" in text, text)
    check("diagnostics name the owning-page rows",
          "'gid': 41" in text, text)

    nothing = trail()
    nothing.arm_from(sample())
    nothing.ingest(sample(action="wander"))
    empty = nothing.diagnostics()
    check("a never-produced run reads as never observed",
          "nowhere -- never observed" in empty, empty)
    check("a never-produced run records no harvest gids",
          "world.harvestFlora: none" in empty, empty)


def main() -> int:
    tests = [value for name, value in sorted(globals().items())
             if name.startswith("test_") and callable(value)]
    for test in tests:
        print(f"{test.__name__}:")
        test()
    print()
    if FAILURES:
        print(f"FAILED ({len(FAILURES)}): " + ", ".join(FAILURES))
        return 1
    print("ALL PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
