#!/usr/bin/env python3
"""The escort fixture's setup gate, driven deterministically (issue #1911).

`tools/item_list_widget_probe.py` is manual-only and needs a GPU, and one
run is about fifteen minutes of real offscreen Vulkan. Its #1251
unit-to-unit scenario stages an escort and a target that must be OUTSIDE
the transfer contract's reach (Chebyshev > 1) at the instant a Mode A
session is created — that separation is the whole reason the checks after
it mean anything. Staging it depends on four live retries over generated
terrain, so the path where it FAILS is the one an ordinary run cannot be
relied on to take. That is exactly the path this file executes, against a
scripted console rather than an engine.

The defect it pins: the reach precondition used to record its result and
throw the Boolean away, and `transfer_session.create` ran on the very next
statement. Five checks were then graded against a pair the probe had
already reported invalid — "the pair opens" passes with no approach at
all when the two are already in reach, and "the target did not move for
the whole of the approach" measures a walk that never happened. The
retained coordinated run of 2026-08-26 did precisely that at a
maximum-axis gap of exactly 1.0, which is why its other observation had
to be filed inconclusive.

What is pinned here:

  * An exhausted staging loop fails at SETUP. The failure is labeled
    `setup:`, counted once, and turns the run's accumulator non-zero —
    so the fixture the probe could not establish is a red run, never a
    green one.
  * `transfer_session.create` is never sent on that path, and none of the
    five separation-dependent checks is printed.
  * The engine is left as a completed run leaves it: running, with the
    session cleared.
  * The failure detail names EVERY attempted destination and both paused
    positions — four attempts, four distinct snapshots, where one pair of
    overwritten variables could only ever produce the last — so an
    exhausted fixture is attributable without a rerun.
  * The positive control is the same scenario over a console that lets
    the gap open: the gate admits the pair and `transfer_session.create`
    IS reached, so the negative result above is a real difference rather
    than a scenario that stops early whatever happens.

No engine, no world, no GPU, no socket: every case here runs in well
under a second.

Usage:
  python3 tools/test_item_list_widget_probe.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import ast
import contextlib
import importlib
import io
import json
import re
import sys
import types
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
import item_list_widget_probe_checks as checks  # type: ignore  # noqa: E402
import item_list_widget_probe_escort as probe  # type: ignore  # noqa: E402
import item_list_widget_probe_oracle as oracle  # type: ignore  # noqa: E402
import item_list_widget_probe_terrain as terrain  # type: ignore  # noqa: E402

# Since #2046 the probe is a facade over owner-scoped modules, so the
# scenario under test and the state it writes live in two different
# ones: `probe` is now the ESCORT module that owns the #1251 scenario and
# its staging, and `checks` owns the accumulator every check writes
# through.
#
# Each module binds its OWN name for `probelib.send`, so patching one
# leaves every other on a real socket. `terrain` is reached by the
# staging itself (`spawn_pair_apart` and `order_target_away` read ground
# through `tile_surface`); `oracle` is reached by the rendered half past
# the gate, which the positive control below stops short of today but
# which is one edit away from being driven here.
PATCHED = (probe, oracle, terrain)


def _console_modules() -> set[str]:
    """Every probe module the scenario under test can reach that holds
    its own console name.

    Computed from the escort module's real import closure rather than
    listed, so a support module added to that closure later is caught
    here — as a loud failure naming it — instead of quietly opening a
    real socket in the middle of a case. The closure is read out of the
    SOURCE rather than off the imported objects: a sibling imported only
    for a constant leaves nothing on the module to trace back to it."""
    seen: set[str] = set()
    pending = [probe.__name__]
    while pending:
        name = pending.pop()
        if name in seen:
            continue
        seen.add(name)
        source = ast.parse((TOOLS / f"{name}.py").read_text(encoding="utf-8"))
        for node in ast.walk(source):
            if (isinstance(node, ast.ImportFrom) and node.module
                    and node.module.startswith("item_list_widget_probe")):
                pending.append(node.module)
            elif isinstance(node, ast.Import):
                pending += [alias.name for alias in node.names
                            if alias.name.startswith("item_list_widget_probe")]
    return {name for name in seen
            if hasattr(importlib.import_module(name), "send")}


_UNPATCHED = _console_modules() - {module.__name__ for module in PATCHED}
assert not _UNPATCHED, (
    "these probe modules the escort scenario can reach hold their own "
    f"console and are not in PATCHED, so a case would reach a real "
    f"socket through them: {sorted(_UNPATCHED)}")

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

PORT = 9428
ANCHOR = (25, -151)

# The five checks that are claims about an approach. None of them may be
# printed by a run whose pair was already in reach (#1911 requirement 4).
SEPARATION_DEPENDENT = (
    "the target stopped where it stood",
    "the source walks over to the held target",
    "the target's position did not move for the WHOLE",
    "both units are held, on their two sides of the one session",
    "the unit-to-unit pair is ONE escort level with two panes",
)

SPAWN_RE = re.compile(r"unit\.spawn\('acolyte',\s*(-?\d+),\s*(-?\d+)")
INFO_RE = re.compile(r"unit\.getInfo\((\d+)\)")
MOVE_RE = re.compile(r"commandMove\((\d+),\s*(-?\d+),\s*(-?\d+)\)")


class FakeConsole:
    """A scripted debug console for the escort scenario's setup path.

    Answers only what that path actually asks and records every
    statement, so "`transfer_session.create` was never sent" is an
    observation about traffic rather than an inference from control flow.

    `close_the_gap` is the whole fixture. With it set, every move order
    aimed at the TARGET parks it at a maximum-axis gap of exactly 1.0
    from the escort — inside the contract's reach, reproducing the
    recorded run — with a per-attempt x offset so the four attempts get
    four distinct bearings and the retry diagnostics have something to
    distinguish. Without it a move order simply arrives, which is the
    separation the scenario needs."""

    def __init__(self, close_the_gap: bool, creates: str = "true") -> None:
        self.close_the_gap = close_the_gap
        self.creates = creates
        self.traffic: list[str] = []
        self.paused = False
        self.session_live = False
        self.pos: dict[int, tuple[float, float]] = {}
        self.spawned: list[int] = []
        self.parked = 0

    # -- the two console entry points the probe reaches the engine through
    def send(self, port: int, lua: str, timeout: float = 10.0,
             expect_result: bool = True, idle: float | None = None) -> str:
        self.traffic.append(lua)
        return self.answer(lua)

    def send_json(self, port: int, lua: str, timeout: float = 10.0,
                  idle: float | None = None):
        raw = self.send(port, lua, timeout=timeout)
        try:
            return json.loads(raw)
        except (TypeError, ValueError):
            return raw

    def set_paused(self, port: int, on: bool, settle: float = 0.3) -> None:
        self.traffic.append(f"engine.setPaused({'true' if on else 'false'})")
        self.paused = bool(on)

    # -- the fixture
    @property
    def source_uid(self) -> int | None:
        return self.spawned[0] if self.spawned else None

    @property
    def target_uid(self) -> int | None:
        return self.spawned[1] if len(self.spawned) > 1 else None

    def apply_move(self, uid: int, tx: float, ty: float) -> None:
        if self.close_the_gap and uid == self.target_uid:
            self.parked += 1
            ex, ey = self.pos[self.source_uid]
            # max(|dx|, |dy|) == 1.0 exactly: in reach, and NOT > 1.0.
            self.pos[uid] = (ex - 0.1 * self.parked, ey - 1.0)
        else:
            self.pos[uid] = (float(tx), float(ty))

    def answer(self, lua: str) -> str:
        spawn = SPAWN_RE.search(lua)
        if spawn:
            uid = 101 + len(self.spawned)
            self.spawned.append(uid)
            self.pos[uid] = (float(spawn.group(1)), float(spawn.group(2)))
            return str(uid)
        info = INFO_RE.search(lua)
        if info:
            x, y = self.pos[int(info.group(1))]
            return json.dumps({"gridX": x, "gridY": y})
        move = MOVE_RE.search(lua)
        if move:
            self.apply_move(int(move.group(1)), float(move.group(2)),
                            float(move.group(3)))
            return "ok"
        if "world.getSurfaceAt(" in lua:
            # `tile_surface` wants >= 3 whitespace-separated fields, a
            # numeric z and a nil fluid.
            return "12 stone null"
        if "currentAction" in lua:
            return "follow_command"
        if "unit.getKnowledge(" in lua:
            return "0.0"
        if "transfer_session" in lua and ".create(" in lua:
            self.session_live = self.creates == "true"
            return self.creates
        if "transfer_session" in lua and ".clear()" in lua:
            self.session_live = False
            return "ok"
        return "ok"


def instant_poll(seconds: float, fn, interval: float = 0.3):
    """`probelib.poll_until` with the waiting taken out: the same
    poll-until-truthy-or-budget-exhausted answer, in no wall clock.

    Deliberately still BOUNDED. A regression that let the gate admit an
    in-reach pair would run the rendered measurement behind it against
    this console, and every wait in there is a minute of real polling —
    so an unbounded stand-in would make such a mutation hang the
    self-test instead of failing it."""
    for _ in range(max(1, int(seconds / max(interval, 0.05)))):
        got = fn()
        if got:
            return got
    return None


@contextlib.contextmanager
def scripted(console: FakeConsole):
    """Point the probe's console, its pause verb, its find_water
    retirement and its waits at `console`, and take the wall clock out.

    `clear_find_water`, `set_paused` and `poll_until` come from
    `probelib` and call ITS `send`, not the probe's, so patching the
    console alone would leave all three reaching for a real socket or a
    real minute.

    Applied across every module the scenario reaches (`PATCHED`), each
    of which imported its own name for these: a module left unpatched
    would open a real socket the moment the staging called into it."""
    replacements = {
        "send": console.send,
        "send_json": console.send_json,
        "set_paused": console.set_paused,
        "clear_find_water": lambda port, uid, seconds=10.0: True,
        "poll_until": instant_poll,
    }
    saved = [(module, name, getattr(module, name))
             for module in PATCHED
             for name in tuple(replacements) + ("time",)
             if hasattr(module, name)]
    for module, name, value in saved:
        if name == "time":
            setattr(module, name,
                    types.SimpleNamespace(sleep=lambda *_a, **_k: None,
                                          time=value.time))
        else:
            setattr(module, name, replacements[name])
    try:
        yield console
    finally:
        for module, name, value in saved:
            setattr(module, name, value)


def run_scenario(console: FakeConsole) -> tuple[str, int]:
    """Drive the real scenario against `console`; answer its printed
    output and how many checks it failed.

    The accumulator is a module global on `checks`, so it is zeroed for
    the run and restored afterwards: a case here must count its OWN
    failures rather than whatever the case before it left behind. It is
    reached through the module on purpose — importing `failures` by name
    would bind a stale 0 that never moves."""
    saved = checks.failures
    checks.failures = 0
    out = io.StringIO()
    try:
        with scripted(console), contextlib.redirect_stdout(out):
            probe.unit_escort_session_scenario(PORT, *ANCHOR)
        return out.getvalue(), checks.failures
    finally:
        checks.failures = saved


def failed_lines(output: str) -> list[str]:
    return [line for line in output.splitlines() if "[FAIL]" in line]


# --------------------------------------------------------------------------
# The negative path: a fixture that never separates
# --------------------------------------------------------------------------
def test_an_unestablished_fixture_fails_at_setup() -> None:
    print("\na staging loop that never opens the gap")
    console = FakeConsole(close_the_gap=True)
    output, failed = run_scenario(console)
    fails = failed_lines(output)

    expect(failed == 1,
           f"exactly one check is counted as failed (got {failed})")
    expect(len(fails) == 1 and "setup:" in fails[0],
           f"and it is labeled a SETUP failure ({fails!r})")
    expect("outside the contract's own reach rule" in "\n".join(fails),
           f"it is the reach precondition that failed ({fails!r})")


def test_the_setup_failure_creates_no_session_and_grades_nothing() -> None:
    print("\nwhat the setup failure did NOT do")
    console = FakeConsole(close_the_gap=True)
    output, _ = run_scenario(console)
    sent = "\n".join(console.traffic)

    expect(".create(" not in sent,
           "transfer_session.create was never sent")
    graded = [name for name in SEPARATION_DEPENDENT if name in output]
    expect(not graded,
           f"no separation-dependent check was graded ({graded!r})")
    expect(console.paused is False,
           "the simulation is left running, not stopped mid-measurement")
    expect(console.session_live is False
           and any(".clear()" in lua for lua in console.traffic),
           "no session is left behind — the scenario cleared one on its "
           "way out, exactly as its later exits do")


def test_the_setup_failure_retains_every_attempt() -> None:
    print("\nthe retry evidence in the failure output")
    console = FakeConsole(close_the_gap=True)
    output, _ = run_scenario(console)
    detail = failed_lines(output)[0]

    for n in (1, 2, 3, 4):
        expect(f"#{n} sent to " in detail,
               f"attempt {n}'s destination is retained ({detail!r})")
    bearings = re.findall(r"#\d sent to \((-?[\d.]+), (-?[\d.]+)\)", detail)
    expect(len(bearings) == 4 and len(set(bearings)) > 1,
           f"the destinations are the ones each attempt really used, not "
           f"one value repeated ({bearings!r})")
    expect(detail.count("escort at (") == 4
           and detail.count("target at (") == 4,
           f"both paused endpoint positions are retained per attempt "
           f"({detail!r})")
    # The four snapshots are the sharp half: before #1911 a single pair of
    # variables held only the LAST one, so four distinct target positions
    # in one line is what a per-attempt record looks like and an
    # overwritten one cannot produce.
    snapshots = re.findall(r"target at \((-?[\d.]+), (-?[\d.]+)\)", detail)
    expect(len(snapshots) == 4 and len(set(snapshots)) == 4,
           f"the four paused snapshots are four DISTINCT observations "
           f"({snapshots!r})")
    expect(detail.count("Chebyshev 1.000") == 4,
           f"each attempt's measured gap is retained ({detail!r})")


def test_the_loop_is_bounded_and_tries_no_new_heuristic() -> None:
    print("\nthe staging loop itself")
    console = FakeConsole(close_the_gap=True)
    with scripted(console), contextlib.redirect_stdout(io.StringIO()):
        console.spawned.extend((101, 102))
        console.pos[101] = (25.0, -151.0)
        console.pos[102] = (25.0, -151.0)
        staging = probe.stage_escort_separation(PORT, 101, 102)
    expect(len(staging.attempts) == 4,
           f"it stops after its four attempts ({len(staging.attempts)})")
    expect(staging.ordered and not staging.separated,
           "the order was taken up every time and the gap never opened")
    expect(staging.last["gap"] == 1.0,
           f"a gap of exactly 1.0 is IN reach, not out of it "
           f"({staging.last['gap']!r})")
    expect(console.paused is True,
           "an exhausted loop hands its LAST pause back to the caller, so "
           "the teardown is what restores the simulation rather than the "
           "loop happening to have resumed it")


# --------------------------------------------------------------------------
# The positive control: the same scenario over a console that separates
# --------------------------------------------------------------------------
def test_a_separated_fixture_reaches_the_measurement() -> None:
    print("\na staging loop that does open the gap")
    # `creates="false"` stops the run at the first check past the gate,
    # which is the point: what is asserted is that the gate ADMITTED the
    # pair and the create statement was reached, without this test having
    # to fake the whole rendered measurement behind it.
    console = FakeConsole(close_the_gap=False, creates="false")
    output, failed = run_scenario(console)
    fails = failed_lines(output)

    expect(not any("setup:" in line for line in fails),
           f"no setup precondition failed ({fails!r})")
    expect(any(".create(" in lua for lua in console.traffic),
           "transfer_session.create IS reached once the pair is separated")
    expect(failed == 1 and "a unit-to-unit Mode A session is created"
           in "\n".join(fails),
           f"the run stops at the refused session, past the gate ({fails!r})")
    expect(console.paused is False and console.session_live is False,
           "that exit restores the engine the same way the setup one does")


def test_the_separation_measure_is_the_contract_s_own() -> None:
    print("\nthe reach measure")
    staging = probe.EscortSeparation()
    staging.record((9.0, 9.0), (0.0, 0.0), (1.0, 1.0))
    expect(not staging.separated, "Chebyshev 1.0 is in reach")
    staging.record((9.0, 9.0), (0.0, 0.0), (0.0, 1.5))
    expect(staging.separated, "Chebyshev 1.5 is out of reach")
    staging.record(None, None, None)
    expect(not staging.ordered and not staging.separated,
           "an order the AI never took up is neither ordered nor separated")
    expect(probe.EscortSeparation().detail() == "no attempt was made",
           "an empty staging says so rather than raising")


def test_the_exit_status_follows_the_accumulator() -> None:
    """A setup failure is only terminal for the SCENARIO; what makes it
    terminal for the RUN is this, so the two are pinned together."""
    print("\nthe run's exit status")
    saved, out = checks.failures, io.StringIO()
    try:
        for count, expected in ((0, 0), (1, 1), (7, 1)):
            checks.failures = count
            with contextlib.redirect_stdout(out):
                got = checks.probe_result()
            expect(got == expected,
                   f"{count} failed check(s) exits {expected} (got {got})")
    finally:
        checks.failures = saved
    expect("1 check(s) FAILED" in out.getvalue()
           and "all checks passed" in out.getvalue(),
           f"each outcome says which it was ({out.getvalue()!r})")


# --------------------------------------------------------------------------
# The split's own contract (#2046)
# --------------------------------------------------------------------------
# The facade is one registered probe over eight libraries. Every claim
# in that sentence is checkable without an engine, so it is checked here
# rather than left to a reviewer re-deriving it from the imports.
FACADE = "item_list_widget_probe"
SCENARIO_MODULES = {f"{FACADE}_endpoints", f"{FACADE}_inventory",
                    f"{FACADE}_escort", f"{FACADE}_nesting"}
SUPPORT_MODULES = {f"{FACADE}_fixtures", f"{FACADE}_oracle",
                   f"{FACADE}_terrain", f"{FACADE}_checks"}

# The load-bearing order (#2046 requirement 6), and the constraints that
# make it load-bearing (requirement 7, including the fifth one the issue
# body's enumeration omits): inventory and knowledge assert their
# fixtures before temperature strips or restocks them; temperature runs
# before item contents, whose first-aid kit it leaves carried; the
# nesting stock lands only after every exact row/count assertion;
# building escort follows every exact cargo assertion; unit-to-unit
# escort is last of all.
SCENARIO_ORDER = (
    "cargo_scenario",
    "knowledge_scenario",
    "unit_endpoint_scenario",
    "unit_inventory_scenario",
    "store_gesture_scenario",
    "temperature_scenario",
    "item_contents_scenario",
    "nesting_stack_scenario",
    "escort_session_scenario",
    "unit_escort_session_scenario",
)


def _module_source(name: str) -> str:
    return (TOOLS / f"{name}.py").read_text(encoding="utf-8")


def _split_modules() -> list[str]:
    return sorted(path.stem for path in TOOLS.glob(f"{FACADE}*.py")
                  if not path.stem.startswith("test_"))


def test_the_facade_is_the_only_registered_probe() -> None:
    """Requirement 2: the extracted modules are libraries. A second
    registration would double the engine boots and put the same checks
    on two schedules."""
    print("\nthe registry still names one probe")
    import probe_runner_registry  # type: ignore  # noqa: PLC0415

    registered = [(key, script) for key, script, *_ in probe_runner_registry.PROBES
                  if FACADE in script or FACADE in key]
    expect(registered == [("item_list_widget", f"{FACADE}.py")],
           f"exactly one registration, of the facade ({registered!r})")
    libraries = [name for name in _split_modules() if name != FACADE]
    expect(len(libraries) == 8, f"eight libraries ({libraries!r})")
    expect(not [name for name in libraries if name.endswith("_probe")],
           f"none of them is named like a registered probe ({libraries!r})")


def test_no_library_boots_an_engine_or_a_world() -> None:
    """Requirement 12: one boot, one world, one port — all of them the
    facade's. A library that booted its own would multiply a fifteen
    minute run by however many modules did it."""
    print("\nno library boots anything")
    for name in _split_modules():
        if name == FACADE:
            continue
        tree = ast.parse(_module_source(name))
        called = {node.func.id for node in ast.walk(tree)
                  if isinstance(node, ast.Call)
                  and isinstance(node.func, ast.Name)}
        imported = {alias.name for node in ast.walk(tree)
                    if isinstance(node, ast.ImportFrom)
                    for alias in node.names}
        forbidden = (called | imported) & {"boot", "quit_engine"}
        expect(not forbidden,
               f"{name} neither boots nor tears down an engine ({forbidden!r})")
        source = _module_source(name)
        expect("Create World" not in source and "getInitProgress" not in source,
               f"{name} does not carry the worldgen bootstrap")
        expect("argparse" not in source,
               f"{name} defines no CLI of its own")


def test_no_scenario_module_imports_a_sibling() -> None:
    """The acceptance's ownership rule: a scenario module reaches its
    peers' shared needs through support, never through the peer. That is
    why `stack_dump`/`level_list_id` and `check_no_duplicate_rows` sit in
    support despite each having one obvious-looking owner."""
    print("\nscenario modules import support, not each other")
    for name in sorted(SCENARIO_MODULES):
        siblings = {node.module for node in ast.walk(ast.parse(_module_source(name)))
                    if isinstance(node, ast.ImportFrom) and node.module
                    and node.module.startswith(FACADE)}
        leaked = siblings & (SCENARIO_MODULES | {FACADE})
        expect(not leaked, f"{name} imports no sibling scenario ({leaked!r})")
        expect(siblings <= SUPPORT_MODULES,
               f"{name} imports only support modules ({sorted(siblings)!r})")
    for name in sorted(SUPPORT_MODULES):
        upward = {node.module for node in ast.walk(ast.parse(_module_source(name)))
                  if isinstance(node, ast.ImportFrom) and node.module
                  and node.module.startswith(FACADE)} & (SCENARIO_MODULES | {FACADE})
        expect(not upward, f"{name} does not import upward ({upward!r})")


def test_the_facade_holds_the_order_and_no_scenario_body() -> None:
    """Requirements 5 and 6: `_run` is where the sequence is readable,
    and it is the whole sequence — a scenario dropped from it is a check
    silently stopping, which nothing else here would notice."""
    print("\nthe facade owns the order")
    tree = ast.parse(_module_source(FACADE))
    defined = [node.name for node in tree.body
               if isinstance(node, (ast.FunctionDef, ast.ClassDef))]
    expect(not [name for name in defined if name.endswith("_scenario")],
           f"the facade holds no scenario body ({defined!r})")
    run = next(node for node in tree.body
               if isinstance(node, ast.FunctionDef) and node.name == "_run")
    # Sorted by position, not by `ast.walk` order: the nesting scenario
    # is called inside a guard (its fixture may not have come out deep
    # enough to nest), and walk yields a nested statement after every
    # top-level one — which would report the guarded call last however
    # the source reads.
    calls = sorted((node for node in ast.walk(run)
                    if isinstance(node, ast.Call)
                    and isinstance(node.func, ast.Name)
                    and node.func.id.endswith("_scenario")),
                   key=lambda node: (node.lineno, node.col_offset))
    called = [node.func.id for node in calls]
    expect(tuple(called) == SCENARIO_ORDER,
           f"the ten scenarios run in the documented order ({called!r})")
    # Requirement 6 as amended: the guard is part of the order. The
    # nesting scenario runs only when its fixture came out stocked deeply
    # enough, and both fixture-staging failures above it end the run
    # rather than grading the scenarios behind them.
    guarded = [node.func.id
               for statement in ast.walk(run)
               if isinstance(statement, ast.If)
               for node in ast.walk(statement.body[0] if statement.body
                                    else statement)
               if isinstance(node, ast.Call) and isinstance(node.func, ast.Name)
               and node.func.id.endswith("_scenario")]
    expect(guarded == ["nesting_stack_scenario"],
           f"the nesting scenario is the one guarded call ({guarded!r})")
    early_exits = [node for node in ast.walk(run)
                   if isinstance(node, ast.Return) and isinstance(node.value,
                                                                  ast.Constant)
                   and node.value.value == 1]
    expect(len(early_exits) == 2,
           f"two staging failures end the run before any scenario "
           f"({len(early_exits)})")


def test_the_accumulator_is_never_imported_by_value() -> None:
    """`from ..._checks import failures` binds a stale 0, so every failed
    check would still print FAIL and the run would still exit 0."""
    print("\nthe accumulator is reached through its module")
    for name in _split_modules():
        by_value = [alias.name
                    for node in ast.walk(ast.parse(_module_source(name)))
                    if isinstance(node, ast.ImportFrom)
                    and node.module == f"{FACADE}_checks"
                    for alias in node.names if alias.name == "failures"]
        expect(not by_value, f"{name} does not import the counter by value")
    expect(checks.failure_count() == checks.failures,
           "the accessor and the attribute answer the same count")


def test_every_fixture_constant_has_one_definition() -> None:
    """Requirement 8: no YAML body, def name, stock list or
    level-addressing expression is restated in a second module."""
    print("\nfixture content is defined once")
    owners: dict[str, list[str]] = {}
    for name in _split_modules():
        for node in ast.parse(_module_source(name)).body:
            if isinstance(node, ast.Assign):
                for target in node.targets:
                    if isinstance(target, ast.Name) and target.id.isupper():
                        owners.setdefault(target.id, []).append(name)
    duplicated = {const: names for const, names in owners.items()
                  if len(names) > 1}
    expect(not duplicated, f"no constant is defined twice ({duplicated!r})")
    fixtures = f"{FACADE}_fixtures"
    for const in ("TEST_BUILDINGS", "TEST_ITEMS", "TEST_UNITS", "CARGO_STOCK",
                  "CARGO_BULK_STOCK", "CARGO_LIST_ID",
                  "ITEM_CONTENTS_LIST_ID", "UNIT_INV_LIST_ID"):
        expect(owners.get(const) == [fixtures],
               f"{const} is owned by the fixtures module "
               f"({owners.get(const)!r})")


def main() -> int:
    selftestlib.parse_verbose()
    test_an_unestablished_fixture_fails_at_setup()
    test_the_setup_failure_creates_no_session_and_grades_nothing()
    test_the_setup_failure_retains_every_attempt()
    test_the_loop_is_bounded_and_tries_no_new_heuristic()
    test_a_separated_fixture_reaches_the_measurement()
    test_the_separation_measure_is_the_contract_s_own()
    test_the_exit_status_follows_the_accumulator()
    test_the_facade_is_the_only_registered_probe()
    test_no_library_boots_an_engine_or_a_world()
    test_no_scenario_module_imports_a_sibling()
    test_the_facade_holds_the_order_and_no_scenario_body()
    test_the_accumulator_is_never_imported_by_value()
    test_every_fixture_constant_has_one_definition()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, "\nAll item_list_widget_probe escort-setup tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
