#!/usr/bin/env python3
"""Engine-free tests for the river-naming probe's page binding (issue #2653).

`tools/river_naming_probe.py` is manual-only: its acceptance is three real
engine boots, and the race it guards is a scheduling one that three
successful runs cannot reliably expose. The half that must not regress
silently is the cheap one, so this drives the probe's real setup wait,
its real `gen_world` and its real phase-3 grading against a scripted
console that models the bug:

  * `world.waitForInit` and `world.getInitProgress` read the ACTIVE
    page, so an already-finished other page answers them at once;
  * `world.show` lands only when the generation queued ahead of it has
    finished, so the requested page registers, activates and finishes at
    different moments;
  * `world.getRivers` answers from whichever page is active.

What is pinned, because each is a way the wait has been or would be
wrong:

  * A finished, active OTHER page never satisfies any of the three
    boundaries (registered, active, done).
  * Activation before `LoadDone` is not setup: the wait keeps going
    until the requested page itself reports phase 3.
  * Each timeout is a `SetupStalled` naming the page and the boundary
    that stalled, and `gen_world` sends no river query before setup
    succeeds.
  * A stalled setup through `main` is an abort with the page and
    boundary, exits unsuccessfully, emits no check event (so the
    prevented checks stay MISSING) and still quits the engine.
  * Phase 3's check details carry the active page id observed with the
    river payload, so a wrong-page answer says which page answered.
  * The probe's declared checks are unchanged.

No engine, no world, no GPU, and no sleeping: the polling is replaced by
a bounded loop that advances the scripted engine instead of the clock.

Usage:
  python3 tools/test_river_naming_probe.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import json
import sys
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
import river_naming_probe as probe  # type: ignore  # noqa: E402

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

PAGE = "unnamed"
OTHER = "named"


class FakeConsole:
    """A scripted debug console with the engine's page semantics.

    `registered_after`, `active_after` and `done_after` are tick counts
    (`None` = never) for the REQUESTED page; the other page is registered,
    active and finished from the start. One tick passes per poll attempt.
    """

    def __init__(self, registered_after=3, active_after=6, done_after=9,
                 rivers_by_page=None):
        self.when = {"registered": registered_after, "active": active_after,
                     "done": done_after}
        self.ticks = 0
        self.sent: list[str] = []
        self.rivers_by_page = rivers_by_page or {}

    def reached(self, boundary: str) -> bool:
        at = self.when[boundary]
        return at is not None and self.ticks >= at

    @property
    def active(self) -> str:
        return PAGE if self.reached("active") else OTHER

    def phase(self, page: str) -> int:
        if page == OTHER:
            return probe.LOAD_DONE_PHASE
        if not self.reached("registered"):
            return 0
        return probe.LOAD_DONE_PHASE if self.reached("done") else 2

    def send(self, port, lua, timeout=10.0, **kwargs):
        self.sent.append(lua)
        if f"world.getDate('{PAGE}')" in lua:
            return "12" if self.reached("registered") else "nil"
        if f"world.getDate('{OTHER}')" in lua:
            return "7"
        if "world.getActiveWorldId" in lua:
            return f'"{self.active}"'
        if "world.getInitProgress" in lua:
            return str(self.phase(self.active))
        return "ok"

    def send_json(self, port, lua, timeout=10.0, **kwargs):
        self.sent.append(lua)
        if "world.getRivers" in lua:
            return {"active": self.active,
                    "rivers": self.rivers_by_page.get(self.active, [])}
        return None

    def poll_until(self, seconds, fn, interval=0.3):
        """The real polling loop's contract, with the console's clock
        advanced per attempt instead of slept."""
        for _ in range(max(1, int(seconds / interval))):
            value = fn()
            if value:
                return value
            self.ticks += 1
        return None


@contextlib.contextmanager
def patched(console: FakeConsole, **extra):
    names = {"send": console.send, "send_json": console.send_json,
             "poll_until": console.poll_until, **extra}
    originals = {name: getattr(probe, name) for name in names}
    for name, value in names.items():
        setattr(probe, name, value)
    try:
        yield
    finally:
        for name, value in originals.items():
            setattr(probe, name, value)


def stalled(console: FakeConsole, **budgets) -> probe.SetupStalled | None:
    with patched(console):
        try:
            probe.wait_page_setup(9000, PAGE, **budgets)
        except probe.SetupStalled as exc:
            return exc
    return None


def test_other_page_never_satisfies_the_requested_pages_setup() -> None:
    # OTHER is registered, active and finished from tick 0 -- exactly the
    # state `world.waitForInit` returned on immediately.
    console = FakeConsole(registered_after=40, active_after=120,
                          done_after=200)
    expect(stalled(console) is None,
           "expected setup to complete once the page itself did")
    expect(console.reached("registered") and console.reached("active")
           and console.reached("done"),
           "setup returned before the requested page registered, became "
           f"active and finished (tick {console.ticks})")
    expect(console.ticks >= 200,
           f"setup returned at tick {console.ticks}, before the requested "
           f"page's own LoadDone at 200")


def test_activation_before_load_done_is_not_setup() -> None:
    console = FakeConsole(registered_after=1, active_after=2, done_after=50)
    expect(stalled(console) is None, "expected setup to complete")
    expect(console.ticks >= 50,
           f"setup returned at tick {console.ticks} with the requested "
           f"page active but still generating (LoadDone at 50)")


def test_each_boundary_timeout_names_the_page_and_the_boundary() -> None:
    cases = (
        ("registered", FakeConsole(registered_after=None)),
        ("active", FakeConsole(registered_after=1, active_after=None)),
        ("done", FakeConsole(registered_after=1, active_after=2,
                             done_after=None)),
    )
    for boundary, console in cases:
        exc = stalled(console, ready_secs=3.0, active_secs=3.0,
                      done_secs=3.0)
        expect(exc is not None, f"{boundary}: expected a SetupStalled")
        if exc is None:
            continue
        expect(exc.page == PAGE and exc.boundary == boundary,
               f"{boundary}: expected page {PAGE!r} and boundary "
               f"{boundary!r}, got {exc.page!r}/{exc.boundary!r}")
        expect(PAGE in str(exc) and boundary.upper() in str(exc),
               f"{boundary}: expected the message to name the page and "
               f"the boundary, got {str(exc)!r}")


def short_budgets(real_wait):
    """`wait_page_setup` with budgets small enough to stall quickly --
    `gen_world` calls it by module name, so rebinding it is seen."""
    return lambda port, page, *a, **kw: real_wait(port, page, 3.0, 3.0, 3.0)


def test_a_stalled_setup_sends_no_river_query() -> None:
    console = FakeConsole(registered_after=1, active_after=2,
                          done_after=None)
    raised = False
    with patched(console, wait_page_setup=short_budgets(
            probe.wait_page_setup)):
        try:
            probe.gen_world(9000, PAGE, 42, 16, named=False)
        except probe.SetupStalled:
            raised = True
    expect(raised, "expected gen_world to raise SetupStalled")
    expect(not any("getRivers" in lua for lua in console.sent),
           "a river query was sent although setup never finished")
    inits = [lua for lua in console.sent if "world.init" in lua]
    shows = [lua for lua in console.sent if f"world.show('{PAGE}')" in lua]
    expect(len(inits) == 1 and len(shows) == 1,
           f"expected one init and one show, got {inits} / {shows}")
    expect(console.sent.index(inits[0]) < console.sent.index(shows[0]),
           "expected the init to be sent before the show")


class RecordingReporter:
    def __init__(self) -> None:
        self.checks: list[tuple[str, bool, dict | None]] = []
        self.aborts: list[tuple[str, dict | None]] = []

    def check(self, check_id, passed, human, detail=None):
        self.checks.append((check_id, passed, detail))
        return passed

    def abort(self, human, detail=None):
        self.aborts.append((human, detail))

    def note(self, human):
        pass

    def close(self):
        pass

    def engine_log_path(self, name, default):
        return default

    def engine_args(self):
        return []


def test_main_reports_a_stalled_setup_as_an_abort_not_a_river_failure() -> None:
    river = {"id": 1, "name": "x", "gloss": "a b", "sx": 0, "sy": 0,
             "mx": 1, "my": 1, "flow": 1, "segs": 2}
    # Page 1 sets up; the second page never reaches LoadDone.
    console = FakeConsole(registered_after=1, active_after=2,
                          done_after=None,
                          rivers_by_page={OTHER: [river], PAGE: [river]})
    rep = RecordingReporter()
    events: list[str] = []
    stall = short_budgets(probe.wait_page_setup)

    def wait(port, page, *a, **kw):
        # The first page ("named") is the scripted OTHER page: ready.
        return None if page == OTHER else stall(port, page)

    saved_argv = sys.argv
    sys.argv = ["river_naming_probe.py"]
    try:
        with patched(
                console, wait_page_setup=wait,
                rivers=lambda port: [river],
                boot=lambda *a, **kw: events.append("boot") or object(),
                quit_engine=lambda *a, **kw: events.append("quit")):
            original = probe.probe_protocol.reporter_from_env
            probe.probe_protocol.reporter_from_env = lambda descriptor: rep
            try:
                rc = probe.main()
            finally:
                probe.probe_protocol.reporter_from_env = original
    finally:
        sys.argv = saved_argv
    expect(rc == 1, f"expected a nonzero exit, got {rc}")
    expect(len(rep.aborts) == 1,
           f"expected exactly one abort, got {rep.aborts}")
    if rep.aborts:
        human, detail = rep.aborts[0]
        expect(detail == {"page": PAGE, "boundary": "done"},
               f"expected the page and boundary in the detail, got {detail}")
        expect(PAGE in human and "DONE" in human,
               f"expected the diagnostic to name page and boundary, "
               f"got {human!r}")
    graded = [c[0] for c in rep.checks]
    expect(not any(c.startswith("unnamed_") for c in graded),
           f"unnamed_* checks must stay MISSING, got {graded}")
    expect(events == ["boot", "quit"],
           f"expected the engine to be quit after the abort, got {events}")


def test_phase_3_failure_detail_records_the_page_that_answered() -> None:
    first = [{"id": 1, "name": "N", "gloss": "a b"}]
    # The bug: the "unnamed" page is graded but the ACTIVE page is still
    # the named one, so the payload carries names the unnamed page never
    # would.
    console = FakeConsole(registered_after=0, active_after=None,
                          done_after=0, rivers_by_page={OTHER: first})
    rep = RecordingReporter()
    failures: list[str] = []
    with patched(console):
        probe.grade_unnamed_page(rep, 9000, PAGE, first, failures)
    by_id = {c[0]: c for c in rep.checks}
    expect(sorted(by_id) == ["unnamed_absent_names", "unnamed_identity",
                             "unnamed_same_ids"],
           f"expected the three phase-3 checks, got {sorted(by_id)}")
    absent = by_id["unnamed_absent_names"]
    expect(absent[1] is False,
           "expected unnamed_absent_names to fail on the wrong page's payload")
    detail = absent[2] or {}
    expect(detail.get("active_page") == OTHER
           and detail.get("graded_page") == PAGE
           and detail.get("rivers") == first,
           f"expected the active page and payload in the detail, got "
           f"{detail}")
    json.dumps(detail)  # structured details must be serializable
    expect(any(repr(OTHER) in f for f in failures),
           f"expected the standalone failure text to name the active "
           f"page, got {failures}")
    for check_id in ("unnamed_identity", "unnamed_same_ids"):
        expect((by_id[check_id][2] or {}).get("active_page") == OTHER,
               f"expected {check_id} to carry the active page too")


def test_the_declared_checks_are_unchanged() -> None:
    expect([c[0] for c in probe.PROBE_CHECKS] == [
        "rivers_present", "named_identity", "stable_geometry",
        "river_names", "unnamed_identity", "unnamed_same_ids",
        "unnamed_absent_names", "save_request", "save_completed",
        "load_published", "restored_rivers", "regenerated_rivers"],
        "the probe's declared check identifiers or order changed")


def main() -> int:
    selftestlib.parse_verbose()
    test_other_page_never_satisfies_the_requested_pages_setup()
    test_activation_before_load_done_is_not_setup()
    test_each_boundary_timeout_names_the_page_and_the_boundary()
    test_a_stalled_setup_sends_no_river_query()
    test_main_reports_a_stalled_setup_as_an_abort_not_a_river_failure()
    test_phase_3_failure_detail_records_the_page_that_answered()
    test_the_declared_checks_are_unchanged()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, "\nAll river_naming_probe page-binding tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
