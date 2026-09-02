#!/usr/bin/env python3
"""Self-test component: engine render/boot modes, the event-log
progress oracle, and the critic-facing evidence path (#2040).

Owns render-mode threading (#650 — which boot flags each mode maps
to, and the rejection of an unknown one), the event-log store's
sequence arithmetic and every gap it must report (#699/#1714), and
the post-step evidence a turn keeps for the critic (#775/#1752).

Offline: FakeEngine and pure oracle state, no window, no build, no
engine boot, no network, no model call. `RealEngine` is only ever
constructed against os.devnull to read its boot flags, and is the
base class the event-log fakes subclass — the render-mode checks and
the event-log checks share this component for exactly that reason."""
from __future__ import annotations

import os
import sys
import tempfile

_HERE = os.path.dirname(os.path.abspath(__file__))
if _HERE not in sys.path:
    sys.path.insert(0, _HERE)

from engine import EngineCrash, FakeEngine  # noqa: E402
from engine import PlaytestEngine as RealEngine  # noqa: E402
from session import run_replay, run_session  # noqa: E402
from trace import SessionTrace, load_meta, load_turns  # noqa: E402
import agent as agent_mod  # noqa: E402

NAME = "engine"


def run(check) -> None:
    """Run every engine-oracle/evidence check into `check`."""
    with tempfile.TemporaryDirectory() as tmp:
        # 4. render-mode threading (#650): the launcher maps each mode
        # to the right boot flags, rejects unknown modes, and the fake
        # engine (which never boots) stays mode-agnostic.
        check("windowed render mode boots with no mode flags",
              RealEngine(0, os.devnull).boot_mode() == ())
        check("offscreen render mode boots with --offscreen",
              RealEngine(0, os.devnull,
                         render_mode="offscreen").boot_mode() == ("--offscreen",))
        try:
            RealEngine(0, os.devnull, render_mode="fullscreen")
            check("unknown render mode rejected", False)
        except ValueError:
            check("unknown render mode rejected", True)

        # 7. event-log progress (#699, #1714): the engine-side store
        # appends, moves coalesced updates to the tail, and drops from the
        # head at capacity. Every row carries a store-assigned `sequence`
        # -- consecutive from 1, in commit order -- so progress is
        # arithmetic on identities rather than a guess from row values.
        # Exercised through the real oracle state tracker, not only the
        # pure helper.
        from engine import EVENT_LOG_PROGRESS_LUA

        class EventLogEngine(RealEngine):
            """Replays a scripted series of event-log observations.

            Each entry is either a list of rows -- whose own highest
            sequence is then the store's high-water mark, which is what
            an un-reset ring always reports -- or an explicit
            `(rows, high_water)` pair, for the load-publish shape where
            the counter has run ahead of the surviving rows.
            """

            def __init__(self, logs):
                self.logs = iter([self._shape(entry) for entry in logs])
                super().__init__(0, os.devnull)

            @staticmethod
            def _shape(entry):
                if not isinstance(entry, (list, tuple)):
                    # A raw reply, passed through verbatim: the
                    # malformed-shape cases below need the engine to
                    # answer with a Lua nil, an error string or a
                    # partial table, none of which this fixture should
                    # tidy up on the way past.
                    return entry
                if isinstance(entry, tuple):
                    rows, high_water = entry
                else:
                    rows = entry
                    # Only well-formed sequences count: the malformed-row
                    # cases below deliberately ship rows the contract
                    # rejects, and shaping them must not raise before
                    # the oracle gets its chance to.
                    high_water = max([0] + [r["sequence"] for r in rows
                                            if isinstance(r, dict)
                                            and isinstance(r.get("sequence"),
                                                           int)
                                            and not isinstance(
                                                r.get("sequence"), bool)])
                # An empty Lua array serializes as `{}`, and the empty
                # ring is the case that matters most here, so the
                # fixture reproduces that shape rather than smoothing it.
                return {"rows": rows if rows else {}, "highest": high_water}

            def lua(self, code, timeout=0):
                if code == EVENT_LOG_PROGRESS_LUA:
                    return next(self.logs)
                if code == "return debug.drainActionOutcomes()":
                    return []
                raise AssertionError(f"unexpected oracle call: {code}")

        def row(seq, text, game_time=0.0, count=1):
            """One `engine.getEventLog()` row, shaped like the real one."""
            return {"sequence": seq, "category": "probe", "text": text,
                    "gameTime": game_time, "source": "selftest",
                    "count": count}

        def observe(logs):
            """Drive one engine through `logs`, one oracle read each."""
            eng = EventLogEngine(logs)
            return [eng.oracle_events() for _ in logs]

        repeat = row(1, "repeat")
        stable = row(2, "stable")
        appended = row(3, "appended")
        coalesced = row(4, "repeat", count=2)
        rollover = row(5, "rollover")
        reappended = row(6, "appended")
        reads = observe([
            [repeat, stable],
            [repeat, stable, appended],
            [repeat, stable, appended],
            [stable, appended, coalesced],
            [appended, coalesced, rollover],
            [coalesced, rollover, reappended],
        ])
        deltas = [r["event_log_new"] for r in reads]
        gaps = [r["event_log_gaps"] for r in reads]
        check("event-log first snapshot explicitly reports its full baseline",
              deltas[0] == [repeat, stable] and gaps[0] == [], str(reads[0]))
        check("event-log append reports only the appended row",
              deltas[1] == [appended] and gaps[1] == [], str(reads[1]))
        check("event-log unchanged snapshot reports no duplicate rows",
              deltas[2] == [] and gaps[2] == [], str(reads[2]))
        check("event-log coalesce reports only the replacement row; the row "
              "it superseded was already delivered, so it is not a gap",
              deltas[3] == [coalesced] and gaps[3] == [], str(reads[3]))
        check("event-log rollover reports the new tail row",
              deltas[4] == [rollover] and gaps[4] == [], str(reads[4]))
        check("event-log rollover detects a new row matching an evicted row",
              deltas[5] == [reappended] and gaps[5] == [], str(reads[5]))

        # An INTERMEDIATE coalesced replacement — one whose superseded
        # row was itself committed since the last observation, so the
        # observer never saw it. The replacement is reported, and the
        # sequence it retired is a one-element gap: from the snapshot a
        # superseded sequence is indistinguishable from an evicted one,
        # and the gap object deliberately claims only absence.
        reads = observe([
            [row(1, "a"), row(2, "b")],
            [row(1, "a"), row(2, "b"), row(4, "c", count=2)],
        ])
        check("a coalesce that happened BETWEEN observations reports the "
              "replacement and the superseded sequence as a one-element gap",
              reads[1]["event_log_new"] == [row(4, "c", count=2)]
              and reads[1]["event_log_gaps"] == [
                  {"first_sequence": 3, "last_sequence": 3,
                   "missing_count": 1}],
              str(reads[1]))

        # 7a. #1714's two deterministic value-matching failures. Each is
        # a periodic FULL-CAPACITY snapshot where every row of the new
        # observation is byte-identical to a row of the previous one, so
        # the old longest-prefix-subsequence heuristic reported [] (first
        # case, both new rows lost) or only the last row (second case,
        # the new "A" lost). Sequence progress reports both, and names
        # the evicted sequences as a gap.
        a1, b1, a2, b2 = (row(1, "A"), row(2, "B"), row(3, "A"), row(4, "B"))
        a3, b3 = row(5, "A"), row(6, "B")
        reads = observe([[a1, b1, a2, b2], [a2, b2, a3, b3]])
        check("full-capacity snapshot of byte-identical rows reports both "
              "new rows (the [A,B,A,B] -> [A,B,A,B] loss)",
              reads[1]["event_log_new"] == [a3, b3]
              and reads[1]["event_log_gaps"] == [], str(reads[1]))
        a4, b4, a5, a6 = (row(1, "A"), row(2, "B"), row(3, "A"), row(4, "A"))
        a7, b5 = row(5, "A"), row(6, "B")
        reads = observe([[a4, b4, a5, a6], [a5, a6, a7, b5]])
        check("multi-row repeated rollover reports every new row "
              "(the [A,B,A,A] -> [A,A,A,B] loss)",
              reads[1]["event_log_new"] == [a7, b5]
              and reads[1]["event_log_gaps"] == [], str(reads[1]))

        # 7b. More than `eventStoreCap` mutations between two
        # observations. The ring cannot hold them, so the exactly-once
        # guarantee is bounded to what is still represented -- and every
        # unrepresented committed sequence is reported as an explicit
        # maximal-interval gap rather than silently vanishing.
        cap = 1000
        first = [row(n, f"r{n}") for n in (1, 2)]
        overflowed = [row(n, f"r{n}")
                      for n in range(1_500, 1_500 + cap)]
        reads = observe([first, overflowed])
        check("more than eventStoreCap mutations between observations "
              "reports every retained row exactly once",
              reads[1]["event_log_new"] == overflowed, str(len(overflowed)))
        check("more than eventStoreCap mutations between observations "
              "reports the unrepresented ones as one maximal gap",
              reads[1]["event_log_gaps"] == [
                  {"first_sequence": 3, "last_sequence": 1_499,
                   "missing_count": 1_497}],
              str(reads[1]["event_log_gaps"]))

        # 7c. Two disjoint losses in one interval must stay two
        # intervals, not be merged into one span that overstates the
        # damage between them.
        reads = observe([[row(1, "a")],
                         [row(3, "c"), row(4, "d"), row(7, "g")]])
        check("disjoint losses are reported as separate maximal intervals",
              reads[1]["event_log_gaps"] == [
                  {"first_sequence": 2, "last_sequence": 2, "missing_count": 1},
                  {"first_sequence": 5, "last_sequence": 6, "missing_count": 2}],
              str(reads[1]["event_log_gaps"]))

        # 7d. A load publish empties the ring WITHOUT resetting the
        # store's counter, so an emptied snapshot means one of two
        # different things and the high-water mark is the only thing
        # that tells them apart. Nothing committed since the last read:
        # nothing to report, and no invented gap.
        reads = observe([[row(1, "before"), row(2, "before two")],
                         ([], 2),
                         [row(3, "after")]])
        check("an emptied ring with nothing committed since the last read "
              "reports nothing and manufactures no gap",
              reads[1]["event_log_new"] == []
              and reads[1]["event_log_gaps"] == [], str(reads[1]))
        check("a row emitted after the ring was emptied is reported once, "
              "with no gap back to sequence 1",
              reads[2]["event_log_new"] == [row(3, "after")]
              and reads[2]["event_log_gaps"] == [], str(reads[2]))

        # ...and the case that makes the high-water mark necessary
        # (round-1 review): mutations 3-4 are committed between two
        # reads and then discarded by the load publish, leaving an EMPTY
        # ring. Inferring the ceiling from the surviving rows would find
        # none, report no change at all, and hide those two mutations
        # PERMANENTLY if no later row happened to arrive. The gap is
        # reported at the very next read, exactly once, and the read
        # after it — still empty, still high-water 4 — adds nothing.
        reads = observe([[row(1, "a"), row(2, "b")], ([], 4), ([], 4)])
        check("an emptied ring whose counter ran ahead reports the "
              "discarded mutations as a gap",
              reads[1]["event_log_new"] == []
              and reads[1]["event_log_gaps"] == [
                  {"first_sequence": 3, "last_sequence": 4,
                   "missing_count": 2}],
              str(reads[1]))
        check("that gap is reported exactly once, not on every later "
              "empty read",
              reads[2]["event_log_new"] == []
              and reads[2]["event_log_gaps"] == [], str(reads[2]))

        # A partial tail: rows survive, but the store committed further
        # than the newest of them. Truncating the gap at the last
        # surviving row would silently drop the tail.
        reads = observe([[row(1, "a")], ([row(2, "b")], 5)])
        check("a gap whose tail runs past the newest surviving row is "
              "reported to the high-water mark",
              reads[1]["event_log_new"] == [row(2, "b")]
              and reads[1]["event_log_gaps"] == [
                  {"first_sequence": 3, "last_sequence": 5,
                   "missing_count": 3}],
              str(reads[1]))

        # The same rule at the other end: a BASELINE claims nothing was
        # observed before it, so it reports no gap however far the
        # counter has already run — it simply adopts the high-water mark
        # and reports ordinary rows from there.
        reads = observe([[], [row(1, "first"), row(2, "second")]])
        check("an empty baseline reports no gap and lets the next read "
              "report ordinary rows",
              reads[0]["event_log_new"] == []
              and reads[0]["event_log_gaps"] == []
              and reads[1]["event_log_new"] == [row(1, "first"),
                                                row(2, "second")]
              and reads[1]["event_log_gaps"] == [], str(reads))
        reads = observe([([], 5), [row(6, "after")]])
        check("a baseline taken on an already-emptied ring adopts the "
              "high-water mark instead of reporting a pre-baseline gap",
              reads[0]["event_log_gaps"] == []
              and reads[1]["event_log_new"] == [row(6, "after")]
              and reads[1]["event_log_gaps"] == [], str(reads))

        # 7e. A row without a usable sequence is a LOUD failure. Falling
        # back to value matching here would quietly restore the very
        # behaviour #1714 removed, so the oracle refuses instead.
        from engine import OracleContractError
        for bad, label in ((({"text": "no sequence"},), "a missing sequence"),
                           (({"sequence": "3", "text": "stringy"},),
                            "a string sequence"),
                           (({"sequence": 0, "text": "zero"},),
                            "a non-positive sequence")):
            try:
                EventLogEngine([list(bad)]).oracle_events()
            except OracleContractError:
                raised = True
            else:
                raised = False
            check(f"an event-log row with {label} raises instead of "
                  "falling back to value matching", raised, str(bad))

        # The high-water mark gets the same treatment, and for the same
        # reason: it is the ONLY thing that distinguishes an emptied ring
        # from a store where nothing happened, so a missing or unusable
        # one must fail loudly rather than let a load publication's
        # discarded mutations read as "no change".
        for high_water, label in ((None, "a missing high-water mark"),
                                  ("3", "a string high-water mark"),
                                  (-1, "a negative high-water mark")):
            try:
                EventLogEngine([([row(1, "a")], high_water)]).oracle_events()
            except OracleContractError:
                raised = True
            else:
                raised = False
            check(f"an event-log read with {label} raises", raised,
                  str(high_water))
        try:
            EventLogEngine([("not a table", 1)]).oracle_events()
        except OracleContractError:
            raised = True
        else:
            raised = False
        check("an event-log read whose rows are not an array raises", raised)

        # The REPLY shape gets the same treatment (round-3 review). The
        # console is already known reachable -- `lua()` raises
        # EngineCrash otherwise -- so a reply that comes back and is not
        # a progress table means the API is missing or broken. Reading
        # any of these as "no events, no gaps" would leave the cursor
        # untouched and erase the turn's evidence silently, which is the
        # failure this whole change removes.
        for reply, label in ((None, "a nil reply (no such API)"),
                             ("error: attempt to call a nil value",
                              "a Lua error string"),
                             ({"highest": 2}, "a reply with no rows"),
                             ({"rows": []},
                              "a reply with no high-water mark")):
            try:
                EventLogEngine([reply]).oracle_events()
            except OracleContractError:
                raised = True
            else:
                raised = False
            check(f"an event-log progress read returning {label} raises "
                  "instead of reporting an empty observation", raised,
                  repr(reply))

        # An ARRAY reply cannot be scripted through the fixture (a list
        # entry IS its rows-list shorthand), so the unpacker takes it
        # directly.
        from engine import _event_log_reply
        try:
            _event_log_reply([])
        except OracleContractError:
            raised = True
        else:
            raised = False
        check("an event-log progress read returning a bare array raises",
              raised)

        # ...and a malformed reply must not quietly advance or reset the
        # cursor either: the read failed, so the next successful one
        # still reports everything since the last GOOD read.
        recovering = EventLogEngine([[row(1, "a")], None, [row(2, "b")]])
        first = recovering.oracle_events()
        try:
            recovering.oracle_events()
        except OracleContractError:
            pass
        after = recovering.oracle_events()
        check("a failed progress read leaves the cursor untouched, so the "
              "next good read still reports the row it missed",
              first["event_log_new"] == [row(1, "a")]
              and after["event_log_new"] == [row(2, "b")]
              and after["event_log_gaps"] == [], str(after))

        # 7f. A gap must SURVIVE the whole evidence path: both of a
        # turn's oracle reads (#775's pre-step and post-step drains) are
        # concatenated by `_merge_oracle`, written to the trace, and
        # surfaced to the critic as its own signal, its own friction
        # reason, and its own digest field. A loss the critic cannot see
        # is indistinguishable from an unchanged event log, which is the
        # whole defect.
        class GappyEngine(FakeEngine):
            """Reports one distinct gap on each of turn 1's two reads."""

            def __init__(self):
                super().__init__()
                self._calls = 0

            def oracle_events(self):
                self._calls += 1
                if self._calls == 1:
                    return {"event_log_new": [],
                            "event_log_gaps": [{"first_sequence": 11,
                                                "last_sequence": 14,
                                                "missing_count": 4}],
                            "action_outcomes": []}
                if self._calls == 2:
                    # A bad outcome alongside the loss: without the gap
                    # this turn would be reported as a SILENT failure
                    # ("no user-facing event"), which is an assertion
                    # the incomplete evidence cannot support.
                    return {"event_log_new": [],
                            "event_log_gaps": [{"first_sequence": 20,
                                                "last_sequence": 20,
                                                "missing_count": 1}],
                            "action_outcomes": [{"kind": "probe",
                                                 "outcome": "noop",
                                                 "reason": "nothing to do"}]}
                return {"event_log_new": [], "event_log_gaps": [],
                        "action_outcomes": []}

        gapdir = os.path.join(tmp, "gaps")
        gaptrace = SessionTrace(gapdir, {"mode": "selftest-gaps"})
        gapeng = GappyEngine()
        run_session(gapeng, agent_mod.ScriptedAgent(
            [{"do": "wait", "note": "nothing seemed to happen"}]), gaptrace,
            turns=1, dt=0.0, max_seconds=None, memory_turns=4,
            stuck_k=99, settle=0.0)
        gaptrace.finish("turn_budget_exhausted")
        gapturns = load_turns(gapdir)
        check("both oracle reads' gaps are merged onto the producing turn",
              gapturns[0]["oracle"]["event_log_gaps"] == [
                  {"first_sequence": 11, "last_sequence": 14,
                   "missing_count": 4},
                  {"first_sequence": 20, "last_sequence": 20,
                   "missing_count": 1}],
              str(gapturns[0]["oracle"].get("event_log_gaps")))

        from critic import build_signals, friction_candidates, build_digest
        gapsignals = build_signals(gapdir, gapturns)
        check("the critic reads event_log_gaps as its own signal",
              gapsignals[0]["event_log_gaps"] == [
                  {"first_sequence": 11, "last_sequence": 14,
                   "missing_count": 4},
                  {"first_sequence": 20, "last_sequence": 20,
                   "missing_count": 1}],
              str(gapsignals[0].get("event_log_gaps")))
        gapcands = friction_candidates({}, gapsignals)
        gapreasons = [r for c in gapcands for r in c["reasons"]]
        check("a gap with no surviving row raises its own friction reason "
              "naming the lost count",
              any(r.startswith("event-log-gap:") and "5 committed" in r
                  for r in gapreasons), str(gapreasons))
        # ...but a gap ALONGSIDE retained rows is ordinary coalescing
        # traffic, not friction: a burst of identical events supersedes
        # its own sequences every turn, and a candidate per burst would
        # bury the real losses.
        noisy = dict(gapsignals[0], events=[{"cat": "combat", "text": "hit"}])
        check("a gap alongside retained rows raises no standalone candidate",
              not any(r.startswith("event-log-gap:")
                      for c in friction_candidates({}, [noisy])
                      for r in c["reasons"]),
              str(friction_candidates({}, [noisy])))
        check("a gap downgrades the silent-failure claim to a judge-the-"
              "evidence one, carrying the incompleteness caution",
              not any(r.startswith("silent-failure-join:") for r in gapreasons)
              and any(r.startswith("bad-outcome-join:")
                      and "evidence here is incomplete" in r
                      for r in gapreasons),
              str(gapreasons))
        gapdigest = build_digest({}, gapsignals, gapcands)
        check("the digest the critic model reads carries the gaps",
              "event_log_gaps=" in gapdigest
              and '"first_sequence": 11' in gapdigest, gapdigest)

        # A trace recorded BEFORE #1714 carries no `event_log_gaps` key
        # at all; the critic must read that as "no gap was reported",
        # exactly as it already tolerates legacy `outcomes` and
        # pre-#775 `visual_change`.
        legacy_turn = dict(gapturns[0])
        legacy_turn["oracle"] = {k: v for k, v in gapturns[0]["oracle"].items()
                                 if k != "event_log_gaps"}
        legacy_signals = build_signals(gapdir, [legacy_turn])
        check("a pre-#1714 trace with no event_log_gaps key reads as no gaps",
              legacy_signals[0]["event_log_gaps"] == []
              and not any(r.startswith("event-log-gap:")
                          for c in friction_candidates({}, legacy_signals)
                          for r in c["reasons"]),
              str(legacy_signals[0].get("event_log_gaps")))

        # 8. #775: an event-log row and an F4 outcome that only become
        # readable once the sim step has genuinely run must land on the
        # turn whose action caused them, not the following turn. Planted
        # on turn 1's SECOND oracle_events() call — the post-step drain,
        # never the first (pre-step/settle) drain nor any later turn's.
        class StepEvidenceEngine(FakeEngine):
            def __init__(self):
                super().__init__()
                self._events_calls = 0

            def oracle_events(self):
                self._events_calls += 1
                if self._events_calls == 2:
                    return {"event_log_new": [{"cat": "world", "text": "step landed"}],
                            "action_outcomes": [{"kind": "probe", "outcome": "accepted"}]}
                return {"event_log_new": [], "action_outcomes": []}

        sedir = os.path.join(tmp, "step_evidence")
        setrace = SessionTrace(sedir, {"mode": "selftest-step-evidence"})
        seeng = StepEvidenceEngine()
        run_session(seeng, agent_mod.ScriptedAgent(
            [{"do": "wait"}, {"do": "wait"}]), setrace,
            turns=2, dt=0.0, max_seconds=None, memory_turns=4,
            stuck_k=99, settle=0.0)
        setrace.finish("turn_budget_exhausted")
        seturns = load_turns(sedir)
        check("event/outcome available only once the step ran lands on "
              "the producing turn, not the next",
              seturns[0]["oracle"]["event_log_new"]
              == [{"cat": "world", "text": "step landed"}]
              and seturns[0]["oracle"]["action_outcomes"]
              == [{"kind": "probe", "outcome": "accepted"}]
              and seturns[1]["oracle"]["event_log_new"] == []
              and seturns[1]["oracle"]["action_outcomes"] == [],
              str([t["oracle"] for t in seturns]))

        # 9. #775: a budget-limited final action must retain its OWN
        # post-step screenshot/oracle evidence — not lose it outright
        # for want of a "next turn" to (mis)capture it on. Writes
        # distinct bytes on exactly the 2nd screenshot call (turn 1's
        # post-step frame) so visual_change is checked against real
        # differing bytes, not FakeEngine's one constant PNG (always
        # equal to itself).
        class ChangingFrameEngine(FakeEngine):
            def __init__(self):
                super().__init__()
                self._shots = 0

            def screenshot(self, path, timeout=None):
                self._shots += 1
                data = self._PNG + (b"\x00" if self._shots == 2 else b"")
                with open(path, "wb") as f:
                    f.write(data)
                self.fb_size = (1280, 720)
                return self.fb_size

        fdir = os.path.join(tmp, "final_turn_evidence")
        ftrace = SessionTrace(fdir, {"mode": "selftest-final-turn-evidence"})
        feng = ChangingFrameEngine()
        freason = run_session(feng, agent_mod.ScriptedAgent(
            [{"do": "wait"}, {"do": "wait"}]), ftrace,
            turns=2, dt=0.0, max_seconds=None, memory_turns=4,
            stuck_k=99, settle=0.0)
        ftrace.finish(freason)
        fturns = load_turns(fdir)
        check("budget-limited final turn retains its own post-step "
              "screenshot and oracle evidence",
              freason == "turn_budget_exhausted"
              and fturns[-1]["oracle"].get("post_screenshot") is not None
              and os.path.isfile(os.path.join(
                  fdir, fturns[-1]["oracle"]["post_screenshot"])),
              str(fturns[-1]["oracle"]))
        check("visual_change is derived from THIS turn's own before/after "
              "frames, not a following turn that may not exist",
              fturns[0]["oracle"]["visual_change"] is True
              and fturns[-1]["oracle"]["visual_change"] is False,
              str([t["oracle"].get("visual_change") for t in fturns]))

        # replay gets the same fix: its own last turn must retain its
        # own post-step evidence too. Before #2040 this borrowed the
        # scripted session/replay pair the session component builds;
        # this component now builds its own equivalent pair (identical
        # parameters) so no component depends on another's trace dir.
        rsrc = os.path.join(tmp, "replay_evidence_session")
        rsrc_trace = SessionTrace(rsrc, {"mode": "selftest", "dt": 0.0})
        rsrc_trace.finish(run_session(
            FakeEngine(), agent_mod.ScriptedAgent(), rsrc_trace, turns=5,
            dt=0.0, max_seconds=None, memory_turns=4, stuck_k=3, settle=0.0))
        rdir = os.path.join(tmp, "replay_evidence")
        rtrace = SessionTrace(rdir, {"mode": "selftest-replay"})
        rtrace.finish(run_replay(FakeEngine(), rsrc, rtrace, dt=0.0,
                                 settle=0.0))
        check("replay's own final turn also retains post-step evidence",
              load_turns(rdir)[-1]["oracle"].get("post_screenshot") is not None)

        # 9b. #1752: the post-step drain is DESTRUCTIVE, so a post frame
        # that raises must not cost the turn the evidence the runner
        # already consumed. Before the fix the merge happened only
        # after the screenshot, so a crash there persisted the pre-only
        # merge — while `_count_f4_outcomes` had already counted the
        # dropped outcomes into the session total, leaving the trace
        # internally inconsistent as well as lossy. The engine below
        # plants post-only evidence on turn 1's SECOND oracle_events()
        # call (the post-step drain) and then dies on turn 1's SECOND
        # screenshot() call (the post frame), in both the session and
        # the replay path.
        POST_FRAME_DEATH = "console died taking the post frame"
        POST_EVIDENCE = [{"cat": "world", "text": "step landed"}]
        POST_OUTCOMES = [{"kind": "probe", "outcome": "accepted"}]

        class CrashOnPostFrameEngine(FakeEngine):
            def __init__(self):
                super().__init__()
                self._events_calls = 0
                self._shots = 0

            def oracle_events(self):
                self._events_calls += 1
                if self._events_calls == 2:
                    return {"event_log_new": [{"cat": "world",
                                               "text": "step landed"}],
                            "event_log_gaps": [],
                            "action_outcomes": [{"kind": "probe",
                                                 "outcome": "accepted"}]}
                return {"event_log_new": [], "event_log_gaps": [],
                        "action_outcomes": []}

            def screenshot(self, path, timeout=None):
                self._shots += 1
                if self._shots == 2:
                    raise EngineCrash(POST_FRAME_DEATH)
                return super().screenshot(path)

        def check_retained_post_evidence(label, turns, meta, raised):
            """The one contract both paths hold (#1752): the drained
            evidence survives, the absent frame is stated as a null
            rather than as a turn that never stepped, the original
            crash is what propagated and ended the session, and the F4
            running total matches what the turn actually retained."""
            oracle = (turns[0].get("oracle") or {}) if turns else {}
            step_phase = turns[0].get("step_phase") if turns else None
            check(f"{label}: a failed post frame keeps the turn's drained "
                  "events and outcomes",
                  len(turns) == 1
                  and oracle.get("event_log_new") == POST_EVIDENCE
                  and oracle.get("action_outcomes") == POST_OUTCOMES,
                  str(oracle))
            check(f"{label}: the missing post frame is represented, and the "
                  "step still reads as completed",
                  oracle.get("post_screenshot") is None
                  and oracle.get("visual_change") is False
                  and step_phase == "completed",
                  f"{oracle}; step_phase {step_phase!r}")
            check(f"{label}: the original EngineCrash propagates and the "
                  "session ends as engine_crash",
                  isinstance(raised, EngineCrash)
                  and str(raised) == POST_FRAME_DEATH
                  and meta.get("stop_reason") == "engine_crash",
                  f"{raised!r}; {meta.get('stop_reason')!r}")
            check(f"{label}: the F4 running total equals the outcomes the "
                  "turn record actually retains",
                  meta.get("f4_outcomes_total")
                  == len(oracle.get("action_outcomes") or []),
                  f"{meta.get('f4_outcomes_total')} counted vs "
                  f"{len(oracle.get('action_outcomes') or [])} retained")

        pfdir = os.path.join(tmp, "crash_post_frame")
        pftrace = SessionTrace(pfdir, {"mode": "selftest-crash-post-frame"})
        pfraised = None
        try:
            run_session(CrashOnPostFrameEngine(), agent_mod.ScriptedAgent(
                [{"do": "wait"}, {"do": "wait"}]), pftrace,
                turns=2, dt=0.0, max_seconds=None, memory_turns=4,
                stuck_k=99, settle=0.0)
        except EngineCrash as e:
            pfraised = e
        pftrace.finish("engine_crash")
        check_retained_post_evidence("session", load_turns(pfdir),
                                     load_meta(pfdir), pfraised)

        # The same for replay, driven from the session trace just
        # recorded above: its one turn is "completed", so replay steps
        # and takes its own post frame — and dies on it the same way.
        rpfdir = os.path.join(tmp, "crash_post_frame_replay")
        rpftrace = SessionTrace(rpfdir,
                                {"mode": "selftest-crash-post-frame-replay"})
        rpfraised = None
        try:
            run_replay(CrashOnPostFrameEngine(), pfdir, rpftrace,
                       dt=0.0, settle=0.0)
        except EngineCrash as e:
            rpfraised = e
        rpftrace.finish("engine_crash")
        check_retained_post_evidence("replay", load_turns(rpfdir),
                                     load_meta(rpfdir), rpfraised)

