#!/usr/bin/env python3
"""Headless probe for issue #1713: a launched lunge must reach its
LANDING STRIKE, not its timeout.

THE DEFECT. `scripts/unit_ai_combat_lunge.lua`'s phase 2 (before #1713 it
lived in `unit_ai_combat_attack.lua`) only strikes once it has seen the
leap in the air -- `lungeSawAir`. But a leap is `TransitioningTo Falling`,
which `activityLabel` (`src/Unit/Thread.hs`) reports to Lua as
`activity == "transitioning"`, and `unit_ai.lua`'s `tickOne` returns on
exactly that activity so an AI command cannot clobber a pose transition.
The only writer of `lungeSawAir` therefore sat downstream of a guard that
was guaranteed to be closed for the whole flight. The flag could never
become true, the strike could never fire, and every lunge ended at
`LUNGE_TIMEOUT_SEC` having delivered nothing -- discarding the reach bonus
and impact speed the lunge exists to apply.

WHY THE EXISTING PROBES DON'T COVER IT. `tools/mental_state_probe.py`'s
lash-out stages spawn ACOLYTES, whose ordinary melee reach never sends
them down the lunge path at all, and it asserts that *an* attack landed
rather than that the lunge's strike did -- an ordinary close-range swing
after the timeout satisfies it. `tools/combat_anim_probe.py` mentions the
lunge only in a comment about telling elevation from pose. Nothing in the
tree drove a SHORT-REACH species through leap -> land -> strike.

THE SPECIES MATTERS. The subject here is a `red_squirrel`: attack range
~0.12 tiles, so it can essentially never be "in reach" of anything and
`tryLunge` is the path it takes, and intelligence ~0.2, which puts it in
`shouldLunge`'s instinct regime (propensity 0.85) rather than the
skill-gated technique one. An acolyte fixture would prove nothing here.

WHAT IS OBSERVED, AND WHY IT IS THE `combat.attack` BOUNDARY. The claim
under test is about the ARGUMENTS of one call: the launched target, the
lunge's stored mode, its stored strike-reach and its stored impact speed.
`unit.getLastAttacker` cannot establish any of that -- it records only
that some attacker landed something -- and `Combat.Resolution.Events`
exposes a boolean `lunge` marker, not the reach value. So this probe
wraps `combat.attack` in Lua and grades the recorded argument tuples,
which is the exact record the assertion needs.

The wrapper RECORDS AND DOES NOT DELEGATE. That is deliberate on both
counts. Resolving the strike would kill the squirrel within a couple of
seconds (an acolyte target retaliates and wins), and it would stamp
`uiLastAttackerUid`, opening the mid-fight retaliation branch and
replacing the target under the very binding the cancellation cases exist
to test. Neither is a property of the lunge. What IS a property of the
lunge is which call it makes with which arguments, and that is upstream of
resolution.

IDENTITY, NOT COUNTING. Every case snapshots the seven lunge fields the
moment the launch is observed and then grades against that lunge's own
identity triple -- (launched target, stored reach, stored impact speed).
That is what separates the landing strike from the two other calls this
same code makes with a positive reach: the `d < 0.5` in-place pounce
(reach = the full `jr.height`) and an ordinary close-range swing (reach
absent). A bare "some attack happened" count could not tell them apart,
which is precisely the weakness of the coverage that already existed.

THE FIVE FIELD-CLEARING CASES. `lungeTarget` is a PERSISTED typed
reference (`scripts/unit_ai_ref_schema.lua`), so a lunge that ends
without clearing its bookkeeping strands a stale unit reference where a
save can pick it up. Every case therefore also asserts that all seven
fields -- read from the module's own `LUNGE_FIELDS` list, so the probe
cannot drift from the code -- are nil once the lunge is over.

  A. success   -- the launched lunge lands and strikes exactly once with
                  its own stored arguments; continued ticks add no second
                  call; the seven fields are cleared.
  B. replaced  -- the attack target is swapped mid-flight. The stored
                  reach and impact speed belong to a leap at the ORIGINAL
                  unit, so the lunge must cancel rather than hand a
                  substitute a free momentum strike -- at the decoy or at
                  anyone else.
  C. timeout   -- the lunge's own clock is aged past LUNGE_TIMEOUT_SEC
                  mid-flight (the engine exposes no way to stall a leap,
                  and a real 3 s wait would still have to be forced to
                  miss its landing). No strike, seven fields cleared.
  D. unlifted  -- `unit.jump` returns true without a leap ever starting.
                  A true return proves only that the command was ENQUEUED
                  (`Engine.Scripting.Lua.API.Units.Spawn`;
                  `Unit.Thread.Command.Motion` can still reject it), so
                  the strike must stay gated on OBSERVED air: no
                  `lungeSawAir`, no strike, and the timeout cleans up.

CASE B GRADES ITS OWN LAUNCH (#2168). Cancelling the replaced lunge hands
the subject straight back to ordinary attack execution against the decoy,
and nothing stops the next eligible tick launching a perfectly legitimate
new lunge at it -- `unit_ai.lua` suppresses a tick only while the unit is
`transitioning`, and the leap has landed by then. Grading "is anything
left over?" as a bare read of the seven fields was therefore unsound in
BOTH directions: a fresh launch inside the grading window repopulated all
seven and was reported as the cancelled launch's leftover bookkeeping,
and a fresh launch inside one poll interval hid the real clear so the
SECOND lunge's cleanup was graded as the first's. This case now closes
both. It DENIES the subject any further jump for the duration -- the
in-flight leap, accepted by the engine ticks earlier, is untouched -- and
it grades residue against the captured launch's own identity
(`lungeTarget` + `lungeStartAt`), attributing the clear only while the
lunge is still inside its own `LUNGE_TIMEOUT_SEC` window, which is the
one window `timedOut` cannot have cleared it in.

EVERY FIXTURE STANDS ON LOADED ARENA TERRAIN (#1909). The flat arena is
only `(2 * arenaRadius + 1)^2` chunks around the origin
(`World.Generate.Arena`) -- global tiles -32..47 on each axis at
chunkSize 16. Outside it `unit.spawn` resolves no height, logs
`unit.spawn: chunk not loaded`, substitutes Z=0 and carries on -- and on
this arena that fallback COINCIDES with the real surface (`arenaZ` is
seaLevel = 0), so a case placed outside the footprint keeps passing
while no longer standing on resolvable arena terrain at all. Two of the
four rows (60 and 90) did exactly that. Every scenario now sits on a row
inside the footprint, every fixture spawn asserts its own tile resolves
a surface BEFORE spawning, and the run refuses to report success unless
the engine's own log agrees no spawn took the fallback. Same shape as
#1395's fix in `tools/bleeding_trail_probe.py`, and like it, local to
this probe: `probelib.spawn_acolyte` and the engine are untouched.

Exit codes: 0 = every declared check passed, 1 = a behavioural failure,
2 = the fixture could not be established (checks left NOT RUN) -- which
includes a spawn tile with no resolvable surface, and an engine log that
either carries the unloaded-chunk warning or cannot be read at all.

Usage:
  python3 tools/lunge_probe.py [--port 9361]
  python3 tools/lunge_probe.py --self-test     # no engine
"""
from __future__ import annotations

import argparse
import glob
import math
import os
import tempfile
import time
from collections.abc import Callable

from probelib import (boot, init_arena, load_ai_stack, poll_until,
                      quit_engine, send, send_json, spawn_acolyte)

LOG = "/tmp/lunge_probe_engine.log"

#: The flat arena's loaded footprint, in global tiles on each axis.
#: `World.Generate.Arena` generates `(2 * arenaRadius + 1)^2` chunks
#: centred on the origin with `arenaRadius = 2` -- chunks -2..2 -- and
#: `chunkSize` is 16 (`World.Chunk.Types`), so tiles -32..47 inclusive.
#: Mirrored by hand, exactly as `bleeding_trail_probe.py` does. A run
#: never TRUSTS these numbers: it asks the engine per tile
#: (`require_loaded_surface`) and then reads the engine's own log
#: (`unloaded_spawn_failure`). They document the layout and drive the
#: no-engine assertions in `--self-test`.
ARENA_MIN_TILE = -32
ARENA_MAX_TILE = 47

#: How far inside that footprint every fixture tile is kept. A leap is at
#: most ~5 tiles for this species (`jumpMaxTiles`,
#: `Unit.Thread.Movement.Leap`, at the red squirrel's capped
#: jumping/agility roll), so this margin keeps a launched subject over
#: loaded terrain for the whole flight rather than only at spawn.
FOOTPRINT_MARGIN = 6

#: Minimum gap between two scenarios' rows: twice that widest leap, so no
#: fixture unit is inside another case's reach and no case can inherit
#: another's landing position, stamina spend or swing cooldown. The
#: per-case fresh-unit isolation `Fixture` documents depends on it.
MIN_ROW_SEPARATION = 12

#: Each scenario's own arena row: (label, row, wants a decoy). All four
#: sit inside the footprint with `FOOTPRINT_MARGIN` to spare and at least
#: `MIN_ROW_SEPARATION` between them -- both asserted, for this exact
#: table, in `--self-test`. Rows 60 and 90, where the timeout and
#: unlifted cases used to stage, were outside the arena entirely (#1909).
CASE_LAYOUT: tuple[tuple[str, int, bool], ...] = (
    ("success",         -22, False),
    ("replaced target",  -2, True),
    ("timeout",          18, False),
    ("unlifted launch",  38, False),
)

#: The warning `unit.spawn` logs when it cannot resolve a spawn height
#: (`Engine.Scripting.Lua.API.Units.Spawn`). It substitutes Z=0 and
#: continues, which on this flat arena silently coincides with the real
#: surface, so the run refuses to report success if the engine took that
#: path even once (#1909 requirement 3).
UNLOADED_SPAWN_WARNING = "unit.spawn: chunk not loaded"

#: What `require_loaded_surface`'s console line returns for a nil
#: `world.getSurfaceAt`. A bare `tostring(nil)` would arrive as the text
#: "nil", indistinguishable from a Lua error message reaching the console.
UNLOADED_REPLY = "unloaded"

#: Every check this probe intends to grade, in report order. A fixture
#: failure reports the ones it never reached as NOT RUN rather than
#: silently omitting them (the accounting `retaliation_swap_probe.py`
#: introduced for the same reason).
DECLARED_CHECKS: tuple[tuple[str, str], ...] = (
    ("strike", "a landed lunge fires its strike instead of timing out"),
    ("args", "the strike carries the launched target, the stored mode, and a "
             "positive stored reach and impact speed"),
    ("once", "the strike resolves at most once — continued ticks add no second call"),
    ("clear_success", "all seven lunge fields are cleared after the strike"),
    ("replaced_nostrike", "a target replaced mid-flight fires no lunge strike"),
    ("replaced_clear", "…and all seven lunge fields are cleared"),
    ("timeout_nostrike", "a lunge aged past its timeout fires no lunge strike"),
    ("timeout_clear", "…and all seven lunge fields are cleared"),
    ("unlifted_noair", "a launch the engine never lifts never observes air"),
    ("unlifted_clear", "…fires no strike and clears all seven lunge fields"),
)

#: How long to keep ticking after the strike while asserting no second
#: call. Phase 2 has NO cooldown gate of its own — a failure to clear the
#: bookkeeping re-fires it on EVERY tick — so a window of this size sees
#: many ticks and does not depend on the attack cooldown's length.
SECOND_CALL_WINDOW = 1.5

#: Bounded re-staging: `shouldLunge` is a probability roll (0.85 for this
#: species) and a leap taken at very close to maximum distance produces a
#: strike-reach near zero, which is a degenerate fixture for the "positive
#: stored reach" assertion rather than a result. Both are re-staged.
LAUNCH_ATTEMPTS = 6

#: Tolerance for matching a captured launch's `lungeStartAt` against a
#: later read of it. Game times are seconds and two launches by the same
#: unit are separated by a whole flight plus an attack cooldown, so this
#: resolves the distinction it exists for with orders of magnitude to
#: spare while surviving the console's JSON round trip of a double.
START_AT_TOL = 1e-3

#: How often case B samples the subject while its cancellation plays out.
#: Tighter than the other polls on purpose: the airborne stretch it has to
#: witness `lungeSawAir` in is a handful of ticks long, and the flag is
#: one of the seven fields the cancellation clears.
CANCEL_POLL = 0.05


class FixtureFailure(RuntimeError):
    """The scenario could not be established, so nothing was graded."""


class Ledger:
    """Declared-vs-recorded check accounting."""

    def __init__(self, declared: tuple[tuple[str, str], ...] = DECLARED_CHECKS):
        self._names = dict(declared)
        self._order = [k for k, _ in declared]
        self._results: dict[str, tuple[bool, str]] = {}
        self._setup_failures: list[str] = []

    def fail_setup(self, message: str) -> None:
        """Record that the run could not be established on arena terrain.

        Held ON the ledger rather than beside it so `report` cannot print
        a success summary over a run that never stood up: a setup failure
        is status 2 whatever the recorded checks say. That matters for
        the end-of-run log scan in particular, which arrives AFTER every
        check has been graded and would otherwise be announced under an
        "all 10 lunge checks passed" line.
        """
        self._setup_failures.append(message)

    def record(self, key: str, passed: bool, detail: str = "") -> None:
        if key not in self._names:
            raise KeyError(f"undeclared check {key!r}")
        self._results[key] = (passed, detail)
        mark = "pass" if passed else "FAIL"
        suffix = f" — {detail}" if detail else ""
        print(f"  [{mark}] {self._names[key]}{suffix}")

    def unrun(self) -> list[str]:
        return [k for k in self._order if k not in self._results]

    def failed(self) -> list[str]:
        return [k for k in self._order if k in self._results
                and not self._results[k][0]]

    def report(self) -> int:
        unrun, failed = self.unrun(), self.failed()
        for message in self._setup_failures:
            print(f"\nFIXTURE FAILURE: {message}")
        if unrun:
            print(f"\nNOT RUN ({len(unrun)}): "
                  + ", ".join(self._names[k] for k in unrun))
        if failed:
            print(f"\n{len(failed)} check(s) FAILED")
        if self._setup_failures:
            return 2
        if failed:
            return 1
        if unrun:
            return 2
        print(f"\nall {len(self._order)} lunge checks passed")
        return 0


# --------------------------------------------------------------------------
# Engine helpers
# --------------------------------------------------------------------------
def lua(*statements: str) -> str:
    """One console line. The debug console is single-line only."""
    return " ".join(statements)


def surface_z(raw, gx: int, gy: int, label: str) -> int:
    """Decode `world.getSurfaceAt`'s console reply, or raise a SETUP failure.

    Split out from the console round trip so `--self-test` can drive
    every reply shape -- an unloaded chunk, a real height, and a garbled
    line -- with no engine.
    """
    if raw == UNLOADED_REPLY:
        raise FixtureFailure(
            f"{label}: no resolvable surface at tile ({gx}, {gy}) — that tile "
            f"is outside the arena's loaded footprint (tiles "
            f"{ARENA_MIN_TILE}..{ARENA_MAX_TILE} on each axis), so unit.spawn "
            "would fall back to Z=0 instead of standing the unit on arena "
            "terrain")
    try:
        return int(float(raw))
    except (TypeError, ValueError):
        raise FixtureFailure(
            f"{label}: world.getSurfaceAt({gx}, {gy}) -> {raw!r}, which is "
            "neither a height nor the unloaded sentinel, so the spawn tile "
            "cannot be shown to carry loaded arena terrain") from None


def require_loaded_surface(port: int, x: float, y: float, label: str) -> int:
    """The spawn tile's surface Z, or a SETUP failure if it has none.

    Asks exactly the question the spawn will ask. `unit.spawn` floors
    both coordinates before calling `surfaceZInWorld`
    (`Engine.Scripting.Lua.API.Units.Spawn`), and `world.getSurfaceAt`
    takes integers and returns nil for an unloaded chunk
    (`Engine.Scripting.Lua.API.WorldQuery.Fluid`), so flooring here poses
    the identical lookup. `getSurfaceAt` returns several values (the
    console tab-joins them); the parentheses keep the first.

    An unresolvable surface is a SETUP failure, never an assertion
    failure: nothing about the lunge has been graded when a fixture
    cannot be placed (#1909 requirement 1).
    """
    gx, gy = math.floor(x), math.floor(y)
    raw = send(port, lua(
        f"local z = (world.getSurfaceAt({gx}, {gy}));",
        f"return z == nil and '{UNLOADED_REPLY}' or tostring(z)"))
    return surface_z(raw, gx, gy, label)


def unloaded_spawn_failure(log: str = LOG) -> str | None:
    """This run's setup-failure message from the engine log, or None.

    The precondition above guards this probe's own spawn sites; this
    reads the engine's own log so a spawn placed outside the footprint by
    any other route fails the run instead of passing quietly (#1909
    requirement 3). A log that cannot be read is itself a setup failure:
    an unreadable log cannot show that every fixture stood on loaded
    terrain, and "could not check" must not be reported as "checked".
    """
    try:
        with open(log, errors="replace") as fh:
            hits = [line.rstrip() for line in fh
                    if UNLOADED_SPAWN_WARNING in line]
    except OSError as exc:
        return (f"could not read the engine log {log} to check for "
                f"{UNLOADED_SPAWN_WARNING!r} ({exc}), so this run cannot show "
                "that every fixture spawned on loaded arena terrain")
    if hits:
        return (f"{len(hits)} spawn(s) fell back to Z=0 on an unloaded chunk — "
                f"every fixture must spawn inside the arena's loaded footprint "
                f"(tiles {ARENA_MIN_TILE}..{ARENA_MAX_TILE} on each axis). "
                f"First: {hits[0]}")
    return None


def bootstrap(port: int) -> None:
    """Register the catalogs the loading screen would, then arena + AI."""
    for pattern, fn in (
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/infections/*.yaml", "engine.loadInfectionYaml"),
        ("data/items/*.yaml", "engine.loadItemYaml"),
        ("data/equipment/*.yaml", "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml", "engine.loadMaterialYaml"),
        ("data/units/*.yaml", "engine.loadUnitYaml"),
    ):
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    load_ai_stack(port)
    init_arena(port)


def install_recorder(port: int) -> None:
    """Wrap `combat.attack` so every call's arguments are captured.

    Record-only, never delegating — see the module docstring: resolving
    the strike would kill the subject and would open the retaliation
    branch that replaces the target under the binding cases B tests.
    """
    send(port, lua(
        "_G.__lg = {};",
        "if not _G.__lg_atk then _G.__lg_atk = combat.attack end;",
        "combat.attack = function(a, t, m, r, sp)",
        " _G.__lg[#_G.__lg + 1] = { atk = a, tgt = t, mode = m or '',",
        "                           reach = r or 0, speed = sp or 0 };",
        " return true end;",
        "return 'ok'"))


def reset_recorder(port: int) -> None:
    send(port, "_G.__lg = {}; return 'ok'")


def calls(port: int) -> list[dict]:
    got = send_json(port, "return _G.__lg")
    return got if isinstance(got, list) else []


def restrict_ai(port: int, ids: list[int]) -> None:
    """Make `unitAi.update` tick exactly `ids`.

    Everything left out is off the AI tick entirely, so a fixture target
    cannot wander out of leap range, fight back, or take its own turn
    while a launch is being staged or graded. `unit.getAllIds` is built
    from `HashMap.keys` with no ordering contract, so spawn order alone
    could not establish this.
    """
    listing = ", ".join(str(i) for i in ids)
    send(port, lua(
        "if not _G.__lg_ids then _G.__lg_ids = unit.getAllIds end;",
        f"unit.getAllIds = function() return {{ {listing} }} end;",
        "return 'ok'"))


def unrestrict_ai(port: int) -> None:
    """Restore the engine's own `unit.getAllIds`.

    A freshly spawned unit only gets AI state from its own `tickOne`, so a
    scenario must lift the previous scenario's restriction before it
    spawns -- otherwise its units never tick and never acquire the state
    every later read depends on.
    """
    send(port, lua(
        "if _G.__lg_ids then unit.getAllIds = _G.__lg_ids;",
        " _G.__lg_ids = nil end;",
        "return 'ok'"))


def stub_jump(port: int, uid: int) -> None:
    """Accept the jump command for `uid` and never lift the unit.

    This is what `Unit.Thread.Command.Motion` rejecting a queued jump
    looks like from Lua: `unit.jump` already returned true.
    """
    send(port, lua(
        "if not _G.__lg_jump then _G.__lg_jump = unit.jump end;",
        f"unit.jump = function(u, ...) if u == {uid} then return true end;",
        " return _G.__lg_jump(u, ...) end;",
        "return 'ok'"))


def deny_jump(port: int, uid: int) -> None:
    """Refuse `uid` any FURTHER jump, leaving the in-flight one alone.

    Case D would otherwise never observe its lunge end: the moment the
    timeout clears the bookkeeping, phase 1 re-launches against the same
    still-accepting stub and the phase reads "air" again. Denying the
    command outright is what the engine does when a leap is impossible, so
    the cancelled lunge is the last one.

    Case B uses it for the same reason (#2168): once the replacement has
    cancelled the lunge, the subject is free to launch a legitimate new
    one at the decoy, and that launch's seven populated fields are not
    the cancelled launch's residue. Only the QUEUING of a new leap is
    refused -- phase 1 sets the bookkeeping inside `if unit.jump(...)`,
    so a denied launch writes nothing, and a leap the engine already
    accepted flies and lands untouched.
    """
    send(port, lua(
        "if not _G.__lg_jump then _G.__lg_jump = unit.jump end;",
        f"unit.jump = function(u, ...) if u == {uid} then return false end;",
        " return _G.__lg_jump(u, ...) end;",
        "return 'ok'"))


def unstub_jump(port: int) -> None:
    send(port, lua(
        "if _G.__lg_jump then unit.jump = _G.__lg_jump; _G.__lg_jump = nil end;",
        "return 'ok'"))


def lunge_fields(port: int) -> list[str]:
    """The module's own field list, so this probe cannot drift from it."""
    got = send_json(port, lua(
        "local L = require('scripts.unit_ai_combat_lunge');",
        "return L.LUNGE_FIELDS"))
    if not isinstance(got, list) or not got:
        raise FixtureFailure(
            "scripts/unit_ai_combat_lunge.lua exposes no LUNGE_FIELDS list, so "
            "the seven-field cleanup cannot be graded against the code's own "
            "definition")
    return [str(f) for f in got]


def lunge_timeout(port: int) -> float:
    """The module's own LUNGE_TIMEOUT_SEC, read like `LUNGE_FIELDS` is.

    Case B needs it to ATTRIBUTE the cleanup it grades. `timedOut` needs
    `now - lungeStartAt > LUNGE_TIMEOUT_SEC`, so bookkeeping that
    disappears while the lunge is still inside that window cannot have
    been cleared by the timeout path in `tryLunge` or `observeTick` — it
    was cleared by the replacement branch, which is the behaviour under
    test.
    """
    got = send_json(port, lua(
        "local L = require('scripts.unit_ai_combat_lunge');",
        "return { sec = L.LUNGE_TIMEOUT_SEC or -1 }"))
    sec = got.get("sec") if isinstance(got, dict) else None
    if not _numeric(sec) or float(sec) <= 0.0:
        raise FixtureFailure(
            "scripts/unit_ai_combat_lunge.lua exposes no positive "
            "LUNGE_TIMEOUT_SEC, so a replaced lunge's cancellation cannot be "
            f"told from its timeout: {got!r}")
    return float(sec)


def lunge_state(port: int, uid: int) -> dict:
    """The whole lunge bookkeeping set plus the pose/activity around it."""
    got = send_json(port, lua(
        "local L = require('scripts.unit_ai_combat_lunge');",
        f"local s = require('scripts.unit_ai').getState({uid});",
        "local left = {};",
        "if s then for _, f in ipairs(L.LUNGE_FIELDS) do",
        " if s[f] ~= nil then left[#left + 1] = f end end end;",
        "return { phase = (s and s.lungePhase) or '',",
        "         saw = (s and s.lungeSawAir) == true,",
        "         target = (s and s.lungeTarget) or -1,",
        "         mode = (s and s.lungeMode) or '',",
        "         reach = (s and s.lungeReach) or -1,",
        "         speed = (s and s.lungeImpactSpeed) or -1,",
        "         startAt = (s and s.lungeStartAt) or -1,",
        "         attackTarget = (s and s.attackTargetUid) or -1,",
        "         now = engine.gameTime() or -1,",
        "         left = table.concat(left, ','),",
        f"         pose = unit.getPose({uid}), activity = unit.getActivity({uid}),",
        "         calls = #(_G.__lg or {}) }"))
    if not isinstance(got, dict):
        raise FixtureFailure(f"could not read unit {uid}'s lunge state: {got!r}")
    return got


def leftover_fields(port: int, uid: int) -> str:
    return str(lunge_state(port, uid).get("left", "?"))


def lunge_strikes(recorded: list[dict], launch: dict) -> list[dict]:
    """The recorded calls that ARE this launch's landing strike.

    Matched on the launch's own identity triple rather than counted: the
    same module also calls `combat.attack` for the in-place pounce (a
    different, larger reach) and for ordinary swings (no reach at all),
    and a count could not tell those from the strike under test.
    """
    return [c for c in recorded
            if c.get("tgt") == launch["target"]
            and _close(c.get("reach"), launch["reach"])
            and _close(c.get("speed"), launch["speed"])]


def momentum_strikes(recorded: list[dict], launch: dict,
                     attacker: int) -> list[dict]:
    """`attacker`'s calls carrying THIS launch's reach and impact speed.

    Deliberately target-INDEPENDENT, unlike `lunge_strikes`, which keeps
    its target-aware meaning for the other scenarios. The call the
    replaced-target case forbids is precisely the free momentum strike
    handed to the SUBSTITUTE, so it names the decoy rather than the
    launched target; matching on the launched target would let exactly
    the forbidden call through (#2168 requirement 2). The stored
    reach/impact-speed pair still separates it from the two other calls
    this module makes: the in-place pounce (reach = the full `jr.height`)
    and an ordinary swing (no reach at all).
    """
    return [c for c in recorded
            if c.get("atk") == attacker
            and _close(c.get("reach"), launch["reach"])
            and _close(c.get("speed"), launch["speed"])]


def same_launch(launch: dict, state: dict) -> bool:
    """Does `state`'s populated bookkeeping belong to THIS launch?

    A launch writes the seven fields as a set and `clear` nils them as a
    set, so `(lungeTarget, lungeStartAt)` identifies which launch a
    populated state came from. Phase 1 is unreachable while
    `lungePhase == "air"`, so a state carrying a DIFFERENT identity is
    proof that the captured launch was cleared before that one started.
    """
    return (state.get("target") == launch["target"]
            and _close(state.get("startAt"), launch["startAt"], START_AT_TOL))


def is_later_launch(launch: dict, state: dict) -> bool:
    """Is `state` populated by a launch that is NOT the captured one?

    Only a launch writes `lungePhase = "air"`, and only alongside a
    target and a start time, so an airborne state carrying a COMPLETE
    identity that is not the captured launch's is another launch. That
    conjunction is deliberately narrow: anything short of it — a
    grounded state, or an airborne one whose identity was partly nilled
    — is exactly the half-cleared bookkeeping the seven-field assertion
    exists to catch, and must not be excused as somebody else's.
    """
    if state.get("phase") != "air":
        return False
    target, started = state.get("target"), state.get("startAt")
    if not _numeric(target) or float(target) < 0:
        return False
    if not _numeric(started) or float(started) < 0:
        return False
    return not same_launch(launch, state)


def launch_residue(launch: dict, state: dict) -> str:
    """The captured launch's OWN leftover fields in `state`, or "".

    This is the distinction #2168 exists for: a cancelled lunge hands the
    subject back to ordinary execution, and a legitimate follow-on launch
    at the replacement fills all seven fields again. That is a new
    lunge's bookkeeping, not the cancelled one's residue, and grading it
    as residue is what made the replaced-target case fail at random.
    """
    left = str(state.get("left", ""))
    if not left or is_later_launch(launch, state):
        return ""
    return left


def _numeric(value) -> bool:
    return isinstance(value, (int, float)) and not isinstance(value, bool)


def _close(a, b, tol: float = 1e-6) -> bool:
    try:
        return abs(float(a) - float(b)) <= tol
    except (TypeError, ValueError):
        return False


# --------------------------------------------------------------------------
# Fixture
# --------------------------------------------------------------------------
class Fixture:
    """One scenario's own subject, target and (optionally) decoy.

    Every scenario stages a FRESH set at its own arena row. Sharing one
    subject across cases would let an earlier case's landing position,
    stamina spend and swing cooldown decide whether a later one can even
    launch. `CASE_LAYOUT` owns the rows, keeps every one of them inside
    the arena's loaded footprint, and keeps them `MIN_ROW_SEPARATION`
    apart so no case's units sit inside another case's leap reach.
    """

    def __init__(self, label: str, row: int, sep: int = 2, decoy: bool = False):
        self.label = label
        self.row = row
        self.sep = sep
        self.wants_decoy = decoy
        self.subject = -1
        self.target = -1
        self.decoy = -1

    def roster(self) -> tuple[tuple[float, float, dict], ...]:
        """(x, y, spawn kwargs) for every unit this fixture stages.

        In spawn order: subject, target, and the decoy when the case
        wants one. `spawn` walks exactly this list, so `--self-test`'s
        footprint assertion covers the tiles a real run uses rather than
        a restatement of them.
        """
        row = float(self.row)
        entries = [(0.0, row, {"unit": "red_squirrel"}),
                   (float(self.sep), row, {"clear_water": False})]
        if self.wants_decoy:
            entries.append((float(-self.sep), row, {"clear_water": False}))
        return tuple(entries)

    def spawn(self, port: int) -> None:
        unrestrict_ai(port)
        placed = [self._stage(port, x, y, kwargs)
                  for x, y, kwargs in self.roster()]
        self.subject, self.target = placed[0], placed[1]
        if self.wants_decoy:
            self.decoy = placed[2]
        # The subject alone: everything else stays off the AI tick, so no
        # fixture unit walks out of leap range or takes a turn of its own
        # while a launch is being staged or graded.
        restrict_ai(port, [self.subject])

    def _stage(self, port: int, x: float, y: float, kwargs: dict) -> int:
        """One fixture unit, refusing a tile with no resolvable surface.

        The precondition runs BEFORE the spawn (#1909 requirement 1):
        `unit.spawn` would otherwise warn, substitute Z=0 and carry on,
        which on this flat arena coincides with the real surface and
        hides the misplacement completely.
        """
        require_loaded_surface(port, x, y, self.label)
        return spawn_acolyte(port, x, y, **kwargs)

    def reach_precondition(self, port: int) -> None:
        """Refuse to grade a subject the lunge path does not apply to."""
        got = send_json(port, lua(
            f"local jr = unit.getJumpReach({self.subject});",
            f"return {{ range = unit.getAttackRange({self.subject}) or -1,",
            f"          intel = unit.getStat({self.subject}, 'intelligence') or -1,",
            "          dist = (jr and jr.dist) or -1,",
            "          height = (jr and jr.height) or -1 }"))
        if not isinstance(got, dict):
            raise FixtureFailure(f"{self.label}: could not read the subject's reach")
        # The whole point of the species choice: a unit whose melee reach
        # already covers the separation would swing, never lunge.
        if got["range"] >= self.sep:
            raise FixtureFailure(
                f"{self.label}: subject melee reach {got['range']:.3f} covers the "
                f"{self.sep}-tile separation, so tryLunge is not the path taken")
        if got["dist"] <= 1.0 or got["height"] <= 0.0:
            raise FixtureFailure(
                f"{self.label}: jump reach dist={got['dist']:.3f} "
                f"height={got['height']:.3f} cannot produce a positive "
                "strike-reach envelope")

    def restage(self, port: int) -> None:
        """Put the subject back at its launch position, stopped and idle."""
        send(port, lua(
            f"unit.stop({self.subject});",
            f"unit.setPos({self.subject}, 0, {self.row});",
            f"unit.setPos({self.target}, {self.sep}, {self.row});",
            "return 'ok'"))

    def command(self, port: int) -> None:
        send(port, lua(
            "local ai = require('scripts.unit_ai');",
            f"ai.commandAttack({self.subject}, {self.target});",
            "return 'ok'"))


def fixture(label: str) -> Fixture:
    """The scenario's fixture, at the row `CASE_LAYOUT` assigns it.

    One table owns every row, so a scenario cannot drift out of the
    arena's footprint without `--self-test` saying so (#1909).
    """
    for name, row, decoy in CASE_LAYOUT:
        if name == label:
            return Fixture(label, row=row, decoy=decoy)
    raise KeyError(f"no arena row declared for scenario {label!r}")


def await_launch(port: int, fx: Fixture, want_positive: bool = True) -> dict:
    """Drive the subject until a lunge is genuinely in flight; snapshot it.

    Returns the launch's stored bookkeeping — the identity every later
    assertion is made against.
    """
    fx.reach_precondition(port)
    for attempt in range(LAUNCH_ATTEMPTS):
        fx.restage(port)
        fx.command(port)
        reset_recorder(port)
        got = poll_until(4.0, lambda: _airborne_snapshot(port, fx.subject),
                         interval=0.1)
        if got is None:
            continue
        if want_positive and (got["reach"] <= 0.0 or got["speed"] <= 0.0):
            # A leap taken at essentially maximum distance flattens the
            # reach envelope to ~0. Re-stage rather than grade "positive"
            # against a degenerate launch.
            poll_until(4.0, lambda: lunge_state(port, fx.subject)["phase"] != "air",
                       interval=0.1)
            continue
        return got
    raise FixtureFailure(
        f"{fx.label}: no lunge launched with usable stored values in "
        f"{LAUNCH_ATTEMPTS} attempts — the subject never entered phase 'air' "
        "with a positive stored reach and impact speed")


def _airborne_snapshot(port: int, uid: int):
    st = lunge_state(port, uid)
    return st if st.get("phase") == "air" else None


# --------------------------------------------------------------------------
# Scenarios
# --------------------------------------------------------------------------
def case_success(port: int, ledger: Ledger, fx: Fixture) -> None:
    fx.spawn(port)
    launch = await_launch(port, fx)
    print(f"  launched: target={launch['target']} mode={launch['mode']!r} "
          f"reach={launch['reach']:.4f} speed={launch['speed']:.4f}")

    landed = await_strike(port, fx.subject, launch)
    if not landed:
        ledger.record("strike", False,
                      "no call carrying the launch's stored reach and impact "
                      "speed appeared before the lunge cleared — the "
                      "pre-#1713 behaviour, where the phase ends at "
                      "LUNGE_TIMEOUT_SEC having struck nothing")
        ledger.record("args", False, "no strike to inspect")
    else:
        ledger.record("strike", True)
        strike = landed[0]
        ok = (strike["tgt"] == launch["target"]
              and strike["mode"] == launch["mode"]
              and launch["reach"] > 0.0 and launch["speed"] > 0.0)
        ledger.record("args", ok,
                      f"tgt={strike['tgt']} mode={strike['mode']!r} "
                      f"reach={strike['reach']:.4f} speed={strike['speed']:.4f}")

    # Continued ticks must not re-fire it. Phase 2 carries no cooldown of
    # its own, so a failure to clear the bookkeeping would strike on every
    # tick in this window.
    time.sleep(SECOND_CALL_WINDOW)
    again = lunge_strikes(calls(port), launch)
    ledger.record("once", len(again) <= 1,
                  f"{len(again)} call(s) carrying this lunge's identity over "
                  f"{SECOND_CALL_WINDOW:.1f}s of continued ticks")

    left = leftover_fields(port, fx.subject)
    ledger.record("clear_success", left == "",
                  f"left set: {left or '(none)'}")


def case_replaced(port: int, ledger: Ledger, fx: Fixture) -> None:
    fx.spawn(port)
    timeout_sec = lunge_timeout(port)
    launch = await_launch(port, fx)
    # Refuse the subject any FURTHER leap, now that the one under test is
    # captured and in the air. The legitimate follow-on lunge at the
    # decoy — which #2168 caught being graded as this launch's leftover
    # bookkeeping — can no longer be launched at all, while the in-flight
    # leap, accepted by the engine before this line ran, is untouched.
    # Installed only after the captured launch and restored below on
    # every exit, so case C stages against the engine's own `unit.jump`.
    deny_jump(port, fx.subject)
    try:
        # Swap the goal's target mid-flight, exactly as the mid-fight
        # retaliation branch does. The stored reach and impact speed
        # describe a leap at the ORIGINAL unit.
        send(port, lua(
            f"local s = require('scripts.unit_ai').getState({fx.subject});",
            f"if s then s.attackTargetUid = {fx.decoy} end;",
            "return 'ok'"))
        _grade_replacement(port, ledger, fx, launch, timeout_sec)
    finally:
        unstub_jump(port)


def case_timeout(port: int, ledger: Ledger, fx: Fixture) -> None:
    fx.spawn(port)
    launch = await_launch(port, fx)
    # The engine offers no way to stall a leap in the air, so age the
    # lunge's OWN clock instead: the field the timeout is measured from.
    send(port, lua(
        "local L = require('scripts.unit_ai_combat_lunge');",
        f"local s = require('scripts.unit_ai').getState({fx.subject});",
        "if s then s.lungeStartAt = engine.gameTime()",
        "                          - (L.LUNGE_TIMEOUT_SEC + 1.0) end;",
        "return 'ok'"))
    _grade_cancellation(port, ledger, fx, launch,
                        clear_key="timeout_clear",
                        nostrike_key="timeout_nostrike")


def case_unlifted(port: int, ledger: Ledger, fx: Fixture) -> None:
    fx.spawn(port)
    stub_jump(port, fx.subject)
    try:
        # No leap ever happens, so the reach envelope is whatever the
        # staged geometry implies; `want_positive` still holds because the
        # stored values are computed before the jump is issued.
        launch = await_launch(port, fx)
        deny_jump(port, fx.subject)
        saw_air = poll_until(
            1.5, lambda: lunge_state(port, fx.subject)["saw"] or None,
            interval=0.1)
        ledger.record("unlifted_noair", saw_air is None,
                      "lungeSawAir stayed false while the unit never left "
                      "the ground" if saw_air is None
                      else "lungeSawAir became true with no leap in progress")
        _grade_cancellation(port, ledger, fx, launch,
                            clear_key="unlifted_clear")
    finally:
        unstub_jump(port)


#: The scenarios this probe runs, in order: (banner, `CASE_LAYOUT`
#: label, case function). Each one is handed the fixture `fixture(label)`
#: builds, so the arena row a case runs on is declared in exactly one
#: place and `--self-test` can hold the layout and the run to the same
#: list of scenarios.
SCENARIOS: tuple[tuple[str, str, Callable[[int, Ledger, Fixture], None]], ...] = (
    ("A. a launched lunge lands and strikes", "success", case_success),
    ("B. the target is replaced mid-flight", "replaced target", case_replaced),
    ("C. the lunge outlives its timeout", "timeout", case_timeout),
    ("D. the engine never lifts the launch", "unlifted launch", case_unlifted),
)


def await_strike(port: int, uid: int, launch: dict,
                 seconds: float = 6.0) -> list[dict]:
    """Poll for this launch's landing strike until the lunge is over.

    Returns as soon as a matching call appears, and also as soon as the
    lunge leaves phase "air" without one -- the pre-#1713 outcome, which
    should report promptly rather than sit out the whole budget.
    """
    deadline = time.time() + seconds
    while time.time() < deadline:
        hits = lunge_strikes(calls(port), launch)
        if hits:
            return hits
        if lunge_state(port, uid)["phase"] != "air":
            return lunge_strikes(calls(port), launch)
        time.sleep(0.1)
    return lunge_strikes(calls(port), launch)


def _num(value) -> str:
    try:
        return f"{float(value):.3f}"
    except (TypeError, ValueError):
        return repr(value)


def _grade_replacement(port: int, ledger: Ledger, fx: Fixture, launch: dict,
                       timeout_sec: float,
                       settle: float = SECOND_CALL_WINDOW) -> None:
    """Grade the cancellation of the CAPTURED launch, and only that one.

    Two independent reads used to decide this — a poll for "not airborne
    any more", then a separate leftover read — with the subject's AI live
    against the replacement in between. Both the residue and the clear it
    was attributed to could belong to a different, perfectly legitimate
    lunge by the time the second read happened (#2168).

    So this walks ONE state read at a time and stops on the captured
    launch's own identity: its residue is gone (`launch_residue`), or the
    lunge has outlived the window in which only the replacement branch
    could have cleared it. Reaching that second exit is the real defect —
    the bookkeeping the replacement was supposed to drop is still sitting
    on the state — so it is recorded as a FAILED check, never as a
    fixture that could not be established. `lungeSawAir` and a grounded,
    non-transitioning read are collected on the way, because a
    cancellation that never followed a real leap and a real landing is
    not the behaviour this case claims to gate.
    """
    started = float(launch["startAt"])
    wall_deadline = time.time() + timeout_sec + settle + 8.0
    saw_air = bool(launch.get("saw"))
    grounded_at: float | None = None
    cleared_at: float | None = None
    state = launch
    left_at_exit = ""
    while time.time() < wall_deadline:
        state = lunge_state(port, fx.subject)
        now = state.get("now", -1.0)
        if not _numeric(now) or float(now) < 0:
            raise FixtureFailure(
                f"{fx.label}: the console reported no game time "
                f"({now!r}), so the cancellation cannot be told from the "
                "lunge's own timeout")
        elapsed = float(now) - started
        saw_air = saw_air or state.get("saw") is True
        # Only ONCE the leap has been seen in the air: at capture the
        # jump is merely enqueued, so the subject is still standing on
        # the ground and an ungated read would call that a landing.
        if (grounded_at is None and saw_air
                and state.get("pose") not in ("falling", "dead", "collapsed")
                and state.get("activity") != "transitioning"):
            grounded_at = elapsed
        left_at_exit = launch_residue(launch, state)
        if left_at_exit == "":
            cleared_at = elapsed
            break
        if elapsed > timeout_sec:
            break            # past the point the lunge's own timeout clears it
        time.sleep(CANCEL_POLL)

    if not saw_air:
        raise FixtureFailure(
            f"{fx.label}: the captured launch never reported lungeSawAir, so "
            "no leap was ever observed in the air and there is no landing "
            "cancellation to grade")
    if cleared_at is None and grounded_at is None:
        raise FixtureFailure(
            f"{fx.label}: the subject never came down "
            f"(pose={state.get('pose')!r} activity={state.get('activity')!r} "
            f"{_num(timeout_sec)}s after the launch), so phase 2 never got the "
            "grounded, non-transitioning tick its replacement cancellation "
            "runs on")

    # Keep ticking past the decision: a momentum strike arriving late is
    # still a momentum strike, and residue that reappears is still
    # residue — `launch_residue` keeps a later lunge out of both.
    time.sleep(settle)
    hits = momentum_strikes(calls(port), launch, fx.subject)
    ledger.record(
        "replaced_nostrike", not hits,
        f"{len(hits)} call(s) by the subject carrying the cancelled lunge's "
        f"stored reach {launch['reach']:.4f} and impact speed "
        f"{launch['speed']:.4f}, at any target")

    final = lunge_state(port, fx.subject)
    residue = launch_residue(launch, final)
    in_time = cleared_at is not None and cleared_at <= timeout_sec
    # A clear seen without a landing in between is `observeTick` tidying
    # up after a death or a collapse, not the replacement branch.
    landed = grounded_at is not None
    if cleared_at is None:
        verdict = (f"its bookkeeping SURVIVED the {_num(timeout_sec)}s window "
                   f"only the replacement branch clears in — left: "
                   f"{left_at_exit or '(none)'}")
    elif not in_time:
        verdict = (f"its bookkeeping only went at {_num(cleared_at)}s, past the "
                   f"{_num(timeout_sec)}s timeout that clears it anyway")
    else:
        verdict = f"its bookkeeping went at {_num(cleared_at)}s"
    if landed:
        verdict += f"; the leap landed at {_num(grounded_at)}s"
    else:
        verdict += ("; no grounded, non-transitioning read ever followed the "
                    "leap, so nothing ran phase 2 "
                    f"(pose={state.get('pose')!r} "
                    f"activity={state.get('activity')!r})")
    ledger.record(
        "replaced_clear", in_time and landed and residue == "",
        f"captured launch: target={launch['target']} "
        f"startAt={_num(launch['startAt'])}; {verdict}; grading read "
        f"{_num(settle)}s later: target={final.get('target')} "
        f"startAt={_num(final.get('startAt'))} "
        f"left={final.get('left') or '(none)'}, of which this launch's: "
        f"{residue or '(none)'}")


def _grade_cancellation(port: int, ledger: Ledger, fx: Fixture, launch: dict,
                        clear_key: str, nostrike_key: str | None = None,
                        settle: float = SECOND_CALL_WINDOW) -> None:
    """Wait out the cancellation, then grade "no strike" + "fields cleared".

    `settle` keeps ticking past the clear: a cancellation that dropped the
    phase but left the rest of the bookkeeping behind would be caught by
    the leftover read, and a strike arriving late would be caught by the
    identity match.
    """
    cleared = poll_until(
        float(settle) + 8.0,
        lambda: lunge_state(port, fx.subject)["phase"] != "air" or None,
        interval=0.1)
    if cleared is None:
        raise FixtureFailure(
            f"{fx.label}: the lunge never left phase 'air', so neither the "
            "no-strike nor the cleanup assertion was reached")
    left = leftover_fields(port, fx.subject)
    hits = lunge_strikes(calls(port), launch)
    if nostrike_key is not None:
        ledger.record(nostrike_key, not hits,
                      f"{len(hits)} call(s) carrying the cancelled lunge's "
                      "identity")
        ledger.record(clear_key, left == "", f"left set: {left or '(none)'}")
        return
    ledger.record(clear_key, not hits and left == "",
                  f"{len(hits)} lunge strike(s); left set: {left or '(none)'}")


# --------------------------------------------------------------------------
# Entry points
# --------------------------------------------------------------------------
def self_test() -> int:
    """Exercise the accounting and the identity match with no engine."""
    failures: list[str] = []

    launch = {"target": 7, "reach": 1.25, "speed": 4.5, "mode": "quick"}
    recorded = [
        {"atk": 1, "tgt": 7, "mode": "quick", "reach": 0.0, "speed": 0.0},
        {"atk": 1, "tgt": 7, "mode": "quick", "reach": 2.07, "speed": 3.1},
        {"atk": 1, "tgt": 7, "mode": "quick", "reach": 1.25, "speed": 4.5},
    ]
    hits = lunge_strikes(recorded, launch)
    if len(hits) != 1 or hits[0]["reach"] != 1.25:
        failures.append("lunge_strikes did not isolate the landing strike from "
                        "the in-place pounce and the ordinary swing")
    if lunge_strikes(recorded, {**launch, "target": 8}):
        failures.append("lunge_strikes matched a call against a different target")

    # #2168 requirement 2: the replaced-target case forbids the free
    # momentum strike WHOEVER it names, so its matcher drops the target
    # test the other cases depend on — and only that test.
    captured = {**launch, "startAt": 100.5}
    at_decoy = {"atk": 1, "tgt": 9, "mode": "quick", "reach": 1.25, "speed": 4.5}
    if len(momentum_strikes(recorded + [at_decoy], captured, 1)) != 2:
        failures.append("momentum_strikes missed the momentum strike handed to "
                        "the substitute, which names the decoy rather than the "
                        "launched target")
    if momentum_strikes([at_decoy], captured, 2):
        failures.append("momentum_strikes matched a call by another attacker")
    if momentum_strikes([{**at_decoy, "reach": 2.07}], captured, 1):
        failures.append("momentum_strikes matched the in-place pounce's larger "
                        "stored reach")
    if momentum_strikes([{**at_decoy, "speed": 3.1}], captured, 1):
        failures.append("momentum_strikes matched a call carrying a different "
                        "impact speed")
    if lunge_strikes(recorded + [at_decoy], captured) != [recorded[2]]:
        failures.append("lunge_strikes stopped being target-aware, which the "
                        "success, timeout and unlifted cases rely on")

    # #2168 requirement 1: residue is the CAPTURED launch's bookkeeping,
    # never a legitimate follow-on lunge's — and everything short of a
    # complete other identity stays residue, because a half-cleared
    # state is what the seven-field assertion is for.
    fields = ("lungePhase,lungeSawAir,lungeStartAt,lungeTarget,lungeMode,"
              "lungeReach,lungeImpactSpeed")
    cleared = {"phase": "", "left": "", "target": -1, "startAt": -1}
    intact = {"phase": "air", "left": fields, "target": 7, "startAt": 100.5}
    relaunch = {"phase": "air", "left": fields, "target": 9, "startAt": 118.25}
    if launch_residue(captured, cleared) != "":
        failures.append("a state with every lunge field nil reported residue")
    if launch_residue(captured, intact) != fields:
        failures.append("the captured launch's own uncleared fields were not "
                        "reported as its residue")
    if launch_residue(captured, {**intact, "startAt": 100.5004}) != fields:
        failures.append("a start time inside START_AT_TOL was read as a "
                        "different launch")
    if launch_residue(captured, relaunch) != "":
        failures.append("a fresh launch at the replacement target was graded as "
                        "the cancelled launch's leftover bookkeeping — the "
                        "#2168 defect")
    if launch_residue(captured, {**relaunch, "target": 7}) != "":
        failures.append("a fresh launch at the SAME target, minutes later, was "
                        "graded as the cancelled launch's residue")
    if launch_residue(captured, {**relaunch, "startAt": 100.5}) != "":
        failures.append("a launch at a different target sharing a start time "
                        "was graded as the cancelled launch's residue")
    # Half-cleared states: not airborne, or airborne with an identity
    # that was itself nilled. None of these is somebody else's launch.
    partial = "lungeTarget,lungeMode,lungeReach,lungeImpactSpeed"
    if launch_residue(captured, {"phase": "", "left": partial, "target": 7,
                                 "startAt": -1}) != partial:
        failures.append("a cancellation that dropped the phase and the start "
                        "time but stranded the persisted lungeTarget was "
                        "excused as a different launch")
    if launch_residue(captured, {"phase": "air", "left": partial, "target": -1,
                                 "startAt": -1}) != partial:
        failures.append("an airborne state whose own identity was nilled was "
                        "excused as a different launch")
    if launch_residue(captured, {"phase": "air", "left": partial, "target": -1,
                                 "startAt": 118.25}) != partial:
        failures.append("an airborne state with a nilled lungeTarget was "
                        "excused as a different launch on its start time alone")
    if launch_residue(captured, {"phase": "air", "left": partial, "target": 9,
                                 "startAt": -1}) != partial:
        failures.append("an airborne state with a nilled lungeStartAt was "
                        "excused as a different launch on its target alone")
    if launch_residue(captured, {"phase": "", "left": fields, "target": 9,
                                 "startAt": 118.25}) != fields:
        failures.append("a GROUNDED state carrying a full field set was "
                        "excused as a different launch, though only a launch "
                        "writes phase 'air'")

    # #1909: the declared layout itself. Every tile a fixture spawns on
    # is inside the arena's loaded footprint with margin to spare, and no
    # case's units sit inside another case's leap reach. Rows 60 and 90
    # satisfied neither, and a real run could not say so.
    rows = [row for _, row, _ in CASE_LAYOUT]
    run_labels = [label for _, label, _ in SCENARIOS]
    if sorted(label for label, _, _ in CASE_LAYOUT) != sorted(run_labels):
        failures.append("CASE_LAYOUT does not declare a row for exactly the "
                        f"scenarios this probe runs: "
                        f"{[l for l, _, _ in CASE_LAYOUT]} vs {run_labels}")
    if len(set(rows)) != len(rows):
        failures.append(f"two scenarios share an arena row: {rows}")
    low = ARENA_MIN_TILE + FOOTPRINT_MARGIN
    high = ARENA_MAX_TILE - FOOTPRINT_MARGIN
    for label, row, decoy in CASE_LAYOUT:
        for x, y, _ in Fixture(label, row=row, decoy=decoy).roster():
            if not (low <= x <= high and low <= y <= high):
                failures.append(f"{label!r} stages a unit at ({x}, {y}), "
                                f"outside the arena footprint's safe band "
                                f"[{low}, {high}] — the #1909 defect")
    for index, first in enumerate(rows):
        for second in rows[index + 1:]:
            if abs(first - second) < MIN_ROW_SEPARATION:
                failures.append(f"rows {first} and {second} are "
                                f"{abs(first - second)} tiles apart, inside "
                                f"MIN_ROW_SEPARATION ({MIN_ROW_SEPARATION}), "
                                "so one case's units are within another's "
                                "leap reach")
    for label, row, decoy in CASE_LAYOUT:
        built = fixture(label)
        if (built.label, built.row, built.wants_decoy) != (label, row, decoy):
            failures.append(f"fixture({label!r}) built "
                            f"({built.label!r}, {built.row}, "
                            f"{built.wants_decoy}) rather than the declared "
                            f"({label!r}, {row}, {decoy})")
    try:
        fixture("no such scenario")
    except KeyError:
        pass
    else:
        failures.append("fixture() invented a row for an undeclared scenario")

    # #1909 requirement 1: the spawn precondition. `surface_z` is the
    # whole decision `require_loaded_surface` makes about a console
    # reply, so every reply shape is graded here with no engine.
    try:
        float(UNLOADED_REPLY)
    except ValueError:
        pass
    else:
        failures.append(f"UNLOADED_REPLY ({UNLOADED_REPLY!r}) parses as a "
                        "height, so a nil world.getSurfaceAt would be read as "
                        "a resolved surface")
    try:
        surface_z(UNLOADED_REPLY, 0, 60, "timeout")
    except FixtureFailure as exc:
        if "footprint" not in str(exc):
            failures.append("an unloaded spawn tile was refused, but not as an "
                            f"out-of-footprint tile: {exc}")
    else:
        failures.append("a spawn tile on an unloaded chunk was accepted as "
                        "resolvable terrain — the #1909 defect")
    try:
        surface_z("attempt to index a nil value", 0, 0, "success")
    except FixtureFailure:
        pass
    else:
        failures.append("a garbled world.getSurfaceAt reply was accepted as a "
                        "surface height")
    for reply, expected in (("0", 0), ("12.0", 12), ("-3", -3)):
        try:
            got = surface_z(reply, 0, 0, "success")
        except FixtureFailure as exc:
            failures.append(f"a resolved surface height {reply!r} was refused "
                            f"as unusable: {exc}")
        else:
            if got != expected:
                failures.append(f"surface_z({reply!r}) decoded to {got!r}, not "
                                f"{expected}")

    # ...and that it runs BEFORE the spawn, so an unresolvable tile can
    # never reach unit.spawn's Z=0 fallback in the first place.
    staged: list[tuple[float, float]] = []

    def _refuse_surface(port, x, y, label):
        raise FixtureFailure(f"{label}: refused ({x}, {y})")

    def _record_spawn(port, x, y, **kwargs):
        staged.append((x, y))
        return 100 + len(staged)

    saved = {name: globals()[name]
             for name in ("require_loaded_surface", "spawn_acolyte")}
    try:
        globals()["require_loaded_surface"] = _refuse_surface
        globals()["spawn_acolyte"] = _record_spawn
        try:
            Fixture("staging", row=0)._stage(0, 0.0, 60.0, {})
        except FixtureFailure:
            pass
        else:
            failures.append("a fixture unit was staged on a tile whose surface "
                            "does not resolve")
        if staged:
            failures.append("unit.spawn was reached despite an unresolvable "
                            "spawn surface — the precondition must run first")
        globals()["require_loaded_surface"] = lambda port, x, y, label: 0
        if Fixture("staging", row=0)._stage(0, 1.0, 2.0, {}) != 101:
            failures.append("a fixture unit with a resolvable surface was not "
                            "spawned")
        if staged != [(1.0, 2.0)]:
            failures.append(f"the staged tile was not the one requested: "
                            f"{staged}")
    finally:
        globals().update(saved)

    # #1909 requirement 3: the end-of-run log scan, which catches a spawn
    # placed outside the footprint by any route the precondition above
    # does not guard.
    with tempfile.TemporaryDirectory() as tmp:
        clean = os.path.join(tmp, "clean.log")
        with open(clean, "w") as fh:
            fh.write("READY\n[Info] unit.spawn: acolyte at (2, -22)\n")
        if unloaded_spawn_failure(clean) is not None:
            failures.append("a clean engine log was read as a fallback spawn")
        dirty = os.path.join(tmp, "dirty.log")
        with open(dirty, "w") as fh:
            fh.write(f"READY\n[Warn] {UNLOADED_SPAWN_WARNING} at (0, 60), "
                     "defaulting Z=0\n")
        if unloaded_spawn_failure(dirty) is None:
            failures.append("an engine log carrying the unloaded-chunk warning "
                            "was read as clean — the #1909 defect")
        if unloaded_spawn_failure(os.path.join(tmp, "absent.log")) is None:
            failures.append("a missing engine log was accepted, so a run that "
                            "could not be checked would report success")

    led = Ledger((("a", "first"), ("b", "second")))
    if led.report() != 2:
        failures.append("a ledger with nothing recorded must report NOT RUN (2)")
    led.record("a", True)
    led.record("b", False)
    if led.report() != 1:
        failures.append("a ledger with a failure must report 1")
    led2 = Ledger((("a", "first"),))
    led2.record("a", True)
    if led2.report() != 0:
        failures.append("a fully-passing ledger must report 0")
    try:
        led2.record("nope", True)
    except KeyError:
        pass
    else:
        failures.append("recording an undeclared check must raise")
    # A setup failure outranks a full sheet of passes: the engine-log
    # scan lands after every check has been graded, and must not be
    # announced under an "all N checks passed" line (#1909 review).
    led3 = Ledger((("a", "first"),))
    led3.record("a", True)
    led3.fail_setup("the engine log could not be read")
    if led3.report() != 2:
        failures.append("a fully-recorded ledger carrying a setup failure "
                        "reported success instead of 2")

    for f in failures:
        print(f"  [FAIL] {f}")
    if failures:
        print(f"\n{len(failures)} self-test failure(s)")
        return 1
    print("self-test: all checks passed")
    return 0


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9361)
    ap.add_argument("--self-test", action="store_true",
                    help="run the no-engine accounting checks and exit")
    args = ap.parse_args()
    if args.self_test:
        return self_test()

    port = args.port
    ledger = Ledger()
    proc = boot(port, log=LOG)
    try:
        bootstrap(port)
        install_recorder(port)
        fields = lunge_fields(port)
        if len(fields) != 7:
            raise FixtureFailure(
                "scripts/unit_ai_combat_lunge.lua declares "
                f"{len(fields)} lunge field(s), not the seven issue #1713's "
                f"requirement 3 names: {fields}")
        print(f"lunge bookkeeping fields: {', '.join(fields)}")

        for banner, label, run_case in SCENARIOS:
            print(f"\n-- {banner} --")
            run_case(port, ledger, fixture(label))
    except FixtureFailure as exc:
        ledger.fail_setup(str(exc))
    finally:
        quit_engine(port, proc)

    # Nothing above can report success until the engine's own log agrees
    # every spawn landed on loaded arena terrain (#1909 requirement 3).
    # Read once the engine is down, so the whole run's output is on disk,
    # and folded into the LEDGER rather than checked beside it — that is
    # what stops a clean sheet of ten checks being announced as passing
    # over a run whose fixtures never stood on the arena.
    unloaded = unloaded_spawn_failure(LOG)
    if unloaded is not None:
        ledger.fail_setup(unloaded)
    return ledger.report()


if __name__ == "__main__":
    raise SystemExit(main())
