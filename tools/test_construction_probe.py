#!/usr/bin/env python3
"""Unit tests for the construction probe's timeout bundle (issue #2172).

`tools/construction_probe.py` is manual-only and scenario-heavy: its real
acceptance is a ten-minute engine run, so the half that must not regress
silently is the pure-Python diagnostic layer #2172 added — the bundle
every expiring construct_job wait captures and prints beneath its FAIL
line.

The property this file exists for is the one a live run cannot be
counted on to exhibit, because it only appears when a wait expires AND
its driving worker is already gone:

  `unitAi.aiState` (`scripts/unit_ai_core.lua:80`) is NEVER pruned when
  an individual unit is destroyed — it is emptied only at teardown or a
  load reconciliation. So `unitAi.getState(uid)` answers for a dead
  worker with its last decision, indefinitely. A bundle that sampled the
  AI unconditionally would print a phase and a `currentAction` beside
  `unit: null`, and #2172's classifier would then name a segment the
  engine no longer has any unit standing in. `construct_timeout_bundle`
  therefore establishes existence FIRST and gates the whole AI family on
  it, a caller's already-taken sample included.

Pinned here, each because it is a way that guarantee would be lost:

  * A live worker reports both worker families; a destroyed one reports
    explicit `null` for BOTH, never a stale phase beside `unit: null`.
  * A destroyed worker is never asked for its AI state at all, so the
    stale table is not merely discarded, it is not read.
  * `unit.getInfo` is queried BEFORE `getState`: existence gates the
    sample rather than being checked after it.
  * A sample handed over by a caller (the phase-1 transition wait keeps
    its last one) is used when the worker is alive and DISCARDED when it
    is gone — the stale-state path the gate exists to close, reachable
    without any engine query at all.
  * Every coordinate the expired predicate queried is retained under its
    own key, including an explicit `null` for a designation that has
    already cleared: "cleared" and "not queried" are different facts,
    and a two-designation predicate cannot be read back from one record.
  * `emit_timeout_bundle` prints NOTHING for a successful poll, so a
    passing run's output is unchanged, and exactly one line per family
    otherwise — so a family the engine had nothing for reads as `null`
    rather than vanishing.
  * The classifier `construct_segment` calls a gone worker gone rather
    than naming its stale phase, which is what keeps the FAIL label
    honest.

No engine, no world, no GPU: every test here answers from a fake console
in well under a second.

Usage:
  python3 tools/test_construction_probe.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import io
import json
import re
import sys
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
import construction_probe as probe  # type: ignore  # noqa: E402

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

PORT = 9377

# The live sample a worker that still exists would answer with, and the
# same record left behind after it is destroyed — byte-identical,
# because that is exactly the problem: nothing about the AI table says
# which of the two it is.
LIVE_AI = {
    "attempt": 1, "constructCandidate": False, "constructJob": True,
    "currentAction": "construct_job", "phase": "fetch", "x": 8, "y": 8,
}
LIVE_UNIT = {
    "defName": "acolyte", "gridX": 4.34, "gridY": 6.48, "gridZ": 0,
    "name": "Orrin Pellan", "page": "move_test",
}
DESIGNATION = {
    "attempt": 1, "category": "structure", "kind": "floor",
    "pack": "dungeon_1", "paid": False, "progress": 0.0,
    "status": "claimed", "x": 8, "y": 8, "z": 0,
}

_COORDS = re.compile(r"getDesignationAt\([^,]+,\s*(-?\d+),\s*(-?\d+)\)")


class FakeConsole:
    """Answers the three queries the bundle makes, and records them.

    Substituting at `send_json` rather than at the probe's own helpers
    means `designation_at` and `construct_job_state` are the real ones:
    a test here fails if the production helper stops asking, asks in the
    wrong order, or asks for a unit it has already established is gone.
    """

    def __init__(self, designations, unit_info, ai_state):
        self.designations = designations
        self.unit_info = unit_info
        self.ai_state = ai_state
        self.queries: list[str] = []

    def send_json(self, port, lua, **_kwargs):
        expect(port == PORT, f"the bundle queried port {port}, not {PORT}")
        self.queries.append(lua)
        match = _COORDS.search(lua)
        if match:
            return self.designations.get((int(match.group(1)),
                                          int(match.group(2))))
        if "unit.getInfo" in lua:
            return self.unit_info
        if "getState" in lua:
            return self.ai_state
        raise AssertionError(f"the bundle made an unexpected query: {lua}")

    def kinds(self) -> list[str]:
        """The query sequence, one token per query, in the order made."""
        out = []
        for lua in self.queries:
            if _COORDS.search(lua):
                out.append("designation")
            elif "unit.getInfo" in lua:
                out.append("getInfo")
            elif "getState" in lua:
                out.append("getState")
        return out


@contextlib.contextmanager
def console(designations=None, unit_info=LIVE_UNIT, ai_state=LIVE_AI):
    """Route the probe's whole console surface at a fake.

    `send` is replaced by a raiser: nothing the bundle does may take the
    non-JSON path, and a helper that started using it would otherwise
    reach a real socket from a test that boots nothing.
    """
    fake = FakeConsole(
        {(8, 8): DESIGNATION} if designations is None else designations,
        unit_info, ai_state)

    def forbidden(*_args, **_kwargs):
        raise AssertionError("the bundle used the raw `send` path")

    original_send_json, original_send = probe.send_json, probe.send
    probe.send_json, probe.send = fake.send_json, forbidden
    try:
        yield fake
    finally:
        probe.send_json, probe.send = original_send_json, original_send


def emitted(bundle) -> str:
    out = io.StringIO()
    with contextlib.redirect_stdout(out):
        probe.emit_timeout_bundle(bundle)
    return out.getvalue()


# --- the bundle's worker families --------------------------------------


def test_a_live_worker_reports_both_families() -> None:
    with console() as fake:
        bundle = probe.construct_timeout_bundle(PORT, [(8, 8)], 7)
    expect(bundle["unit"] == LIVE_UNIT,
           f"a live worker's unit record was {bundle['unit']!r}")
    expect(bundle["aiState"] == LIVE_AI,
           f"a live worker's AI state was {bundle['aiState']!r}")
    expect(fake.kinds() == ["designation", "getInfo", "getState"],
           f"a live worker's query sequence was {fake.kinds()}")


def test_a_destroyed_worker_reports_null_for_both_families() -> None:
    # The AI table still answers — that is the whole hazard.
    with console(unit_info=None, ai_state=LIVE_AI):
        bundle = probe.construct_timeout_bundle(PORT, [(8, 8)], 7)
    expect(bundle["unit"] is None,
           f"a destroyed worker's unit record was {bundle['unit']!r}")
    expect(bundle["aiState"] is None,
           "a destroyed worker's AI state was reported as "
           f"{bundle['aiState']!r}, not null")


def test_a_destroyed_worker_is_never_asked_for_its_ai_state() -> None:
    with console(unit_info=None, ai_state=LIVE_AI) as fake:
        probe.construct_timeout_bundle(PORT, [(8, 8)], 7)
    expect("getState" not in fake.kinds(),
           "a destroyed worker's AI state was still queried: "
           f"{fake.kinds()}")


def test_existence_is_established_before_the_ai_sample() -> None:
    with console() as fake:
        probe.construct_timeout_bundle(PORT, [(8, 8)], 7)
    kinds = fake.kinds()
    expect(kinds.index("getInfo") < kinds.index("getState"),
           f"the AI sample was taken before existence was: {kinds}")


def test_a_handed_over_sample_is_used_while_the_worker_is_alive() -> None:
    handed = dict(LIVE_AI, phase="walking")
    with console() as fake:
        bundle = probe.construct_timeout_bundle(PORT, [(8, 8)], 7,
                                                state=handed)
    expect(bundle["aiState"] == handed,
           f"the handed-over sample was not used: {bundle['aiState']!r}")
    expect("getState" not in fake.kinds(),
           f"a fresh AI sample was taken anyway: {fake.kinds()}")


def test_a_handed_over_sample_is_discarded_when_the_worker_is_gone() -> None:
    # The phase-1 transition wait keeps its last sample and hands it
    # over; taken while the worker was alive, it is exactly as stale as
    # a fresh read of the unpruned table, and no engine query is even
    # needed to leak it.
    handed = dict(LIVE_AI, phase="walking")
    with console(unit_info=None) as fake:
        bundle = probe.construct_timeout_bundle(PORT, [(8, 8)], 7,
                                                state=handed)
    expect(bundle["aiState"] is None,
           "a handed-over sample survived the worker's destruction: "
           f"{bundle['aiState']!r}")
    expect(bundle["unit"] is None,
           f"a destroyed worker's unit record was {bundle['unit']!r}")
    expect("getState" not in fake.kinds(),
           f"the gone worker was queried anyway: {fake.kinds()}")


def test_no_driving_unit_reports_null_for_both_worker_families() -> None:
    with console() as fake:
        bundle = probe.construct_timeout_bundle(PORT, [(8, 8)], None)
    expect(bundle["aiState"] is None and bundle["unit"] is None,
           f"a uid-less bundle reported {bundle!r}")
    expect(fake.kinds() == ["designation"],
           f"a uid-less bundle queried a worker anyway: {fake.kinds()}")


# --- the bundle's designation family ------------------------------------


def test_every_queried_coordinate_is_retained_including_a_cleared_one() -> None:
    with console({(8, 8): DESIGNATION, (9, 8): None}):
        bundle = probe.construct_timeout_bundle(PORT, [(8, 8), (9, 8)], 7)
    designations = bundle["designations"]
    expect(set(designations) == {"8,8", "9,8"},
           f"the queried coordinates were not all retained: {designations}")
    expect(designations["8,8"] == DESIGNATION,
           f"the live record was altered: {designations['8,8']!r}")
    expect("9,8" in designations and designations["9,8"] is None,
           "a cleared designation was dropped instead of retained as "
           f"an explicit null: {designations!r}")


def test_a_negative_coordinate_is_keyed_and_queried_intact() -> None:
    # Phases 6-8 site their designations at negative Y.
    record = dict(DESIGNATION, y=-8)
    with console({(8, -8): record}):
        bundle = probe.construct_timeout_bundle(PORT, [(8, -8)], 7)
    expect(bundle["designations"] == {"8,-8": record},
           f"a negative-Y designation round-tripped as "
           f"{bundle['designations']!r}")


# --- emission -----------------------------------------------------------


def test_a_successful_poll_emits_nothing() -> None:
    expect(emitted(None) == "",
           "a passing poll printed something beneath its PASS line")


def test_every_family_is_emitted_once_even_when_null() -> None:
    with console(unit_info=None):
        bundle = probe.construct_timeout_bundle(PORT, [(8, 8)], 7)
    lines = emitted(bundle).splitlines()
    expect(len(lines) == len(probe.CONSTRUCT_BUNDLE_FAMILIES),
           f"the bundle printed {len(lines)} lines for "
           f"{len(probe.CONSTRUCT_BUNDLE_FAMILIES)} families")
    for family, line in zip(probe.CONSTRUCT_BUNDLE_FAMILIES, lines):
        expect(line.strip().startswith(f"(debug) {family}:"),
               f"family {family!r} was not the line printed for it: {line!r}")
    joined = "\n".join(lines)
    expect("aiState: null" in joined and "unit: null" in joined,
           "a destroyed worker's families did not print as explicit "
           f"nulls:\n{joined}")


def test_emitted_records_are_json_and_key_sorted() -> None:
    with console() as _fake:
        bundle = probe.construct_timeout_bundle(PORT, [(8, 8)], 7)
    for line in emitted(bundle).splitlines():
        payload = line.split(":", 1)[1].strip()
        try:
            decoded = json.loads(payload)
        except ValueError:
            expect(False, f"an emitted family was not JSON: {line!r}")
            continue
        expect(json.dumps(decoded, sort_keys=True) == payload,
               f"an emitted family was not key-sorted JSON: {line!r}")


# --- classification ------------------------------------------------------


def test_a_gone_worker_is_classified_gone_not_given_a_stale_phase() -> None:
    with console(unit_info=None, ai_state=LIVE_AI):
        bundle = probe.construct_timeout_bundle(PORT, [(8, 8)], 7)
    segment = probe.construct_segment(bundle["aiState"])
    expect("fetch" not in segment,
           f"a destroyed worker was reported as standing in {segment!r}")
    expect("no AI state" in segment,
           f"a destroyed worker was classified as {segment!r}")


def test_each_segment_is_named_from_the_state_that_shows_it() -> None:
    expect(probe.construct_segment({**LIVE_AI, "phase": "walking"})
           == "walking", "a walking worker was misclassified")
    expect(probe.construct_segment({"constructJob": False,
                                    "currentAction": "idle"})
           == "no constructJob",
           "a worker holding no job was misclassified")
    expect(probe.construct_segment({"constructJob": True, "phase": False})
           == "unknown phase",
           "a job with no phase was misclassified")


def main() -> int:
    selftestlib.parse_verbose()
    test_a_live_worker_reports_both_families()
    test_a_destroyed_worker_reports_null_for_both_families()
    test_a_destroyed_worker_is_never_asked_for_its_ai_state()
    test_existence_is_established_before_the_ai_sample()
    test_a_handed_over_sample_is_used_while_the_worker_is_alive()
    test_a_handed_over_sample_is_discarded_when_the_worker_is_gone()
    test_no_driving_unit_reports_null_for_both_worker_families()
    test_every_queried_coordinate_is_retained_including_a_cleared_one()
    test_a_negative_coordinate_is_keyed_and_queried_intact()
    test_a_successful_poll_emits_nothing()
    test_every_family_is_emitted_once_even_when_null()
    test_emitted_records_are_json_and_key_sorted()
    test_a_gone_worker_is_classified_gone_not_given_a_stale_phase()
    test_each_segment_is_named_from_the_state_that_shows_it()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, "\nAll construction_probe timeout-bundle tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
