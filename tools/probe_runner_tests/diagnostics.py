#!/usr/bin/env python3
"""Durable progress and failure records (#1768, #1982) (#2130).

Eleven groups over `probe_runner_diagnostics`, the protocol that
survives a probe the runner had to kill:

  progress records round-trip, stay out of the verdict shape, derive the
  in-flight set, survive a forced timeout outside the ordinary tail,
  reach the DEFAULT report, and name every attempt across a parallel
  dispatch and its retry;
  failure records round-trip, stay off the progress channel, name every
  recorded failure exactly once, and keep the failed check visible both
  outside the ordinary tail and through the parallel presentation;
  no probe #1982 repaired still reports a terminal failure only to
  stderr;
  a probe's setup exit is recorded and recoverable.

`ProgressStub`, the record parsers and `REPAIRED_PROBES` are this
family's own; `progress_lines` is shared, because the scheduler's
conflict cases count the same lines.
"""
from __future__ import annotations

import sys
import time
from pathlib import Path

from .support import (
    TOOLS_DIR,
    Tree,
    main_with,
    progress_lines,
)

import probe_runner_diagnostics  # noqa: E402
from selftestlib import expect  # noqa: E402


# --------------------------------------------------------------------------
# Durable progress records and timeout attribution (#1768)
#
# The loss these cases are about is invisible to every other test here: a
# probe's phase output is block-buffered in the child, so a `--timeout`
# SIGKILL discards it and the failure artifact names no phase at all. The
# pure cases below pin the ONE shared convention (`run_probes.py` defines
# it; `persistence_contract_sweep.py` and the runner's own nested-attempt
# records both use it), and the subprocess cases prove it survives a real
# forced termination and reaches the DEFAULT failure presentation on both
# of that presentation's two paths.
# --------------------------------------------------------------------------
class ProgressStub:
    """Stands in for "this line is not a progress record" in a filter."""
    kind = ""


def progress_records(out: str) -> list[probe_runner_diagnostics.ProgressRecord]:
    """Every progress record in some output, in order."""
    return [record for record in
            (probe_runner_diagnostics.parse_progress(line) for line in out.splitlines())
            if record is not None]


def record_pairs(out: str) -> list[tuple[str, str]]:
    """`(kind, identity)` for every progress record, in order."""
    return [(record.kind, record.identity) for record in progress_records(out)]


def test_progress_records_round_trip_and_stay_out_of_the_verdict_shape() -> None:
    print("\n-- a progress record round-trips, and can never be miscounted "
          "as a verdict announcement")
    now = time.time()
    line = probe_runner_diagnostics.format_progress(
        "phase", "engine A", "build the scenario, save 'gen1'",
        elapsed=12.34, now=now)
    record = probe_runner_diagnostics.parse_progress(line)
    expect(record is not None, f"the rendered record parses back ({line!r})")
    expect(record.kind == "phase" and record.identity == "engine A",
           f"with its kind and identity intact (got {record!r})")
    expect(record.detail == "build the scenario, save 'gen1'",
           f"and its detail intact (got {record.detail!r})")
    expect("+12.3s" in record.stamp,
           f"the stamp carries the elapsed offset (got {record.stamp!r})")
    expect(":" in record.stamp,
           f"and a wall-clock time, so two producers sharing one pipe can "
           f"be ordered against each other (got {record.stamp!r})")

    # Free text may contain the field separator; only the first three
    # fields are structural.
    awkward = probe_runner_diagnostics.format_progress(
        "end", "chop (chop_probe.py) attempt 1/2", "FAIL | exit 1",
        elapsed=1.0, now=now)
    expect(probe_runner_diagnostics.parse_progress(awkward).detail == "FAIL | exit 1",
           "a detail containing the separator survives the round trip")
    expect(probe_runner_diagnostics.parse_progress(awkward).identity
           == "chop (chop_probe.py) attempt 1/2",
           "and the identity is not confused by it")

    expect(probe_runner_diagnostics.parse_progress("[3/12] chop_probe.py ... PASS (4.0s)")
           is None,
           "an ordinary verdict announcement is not a progress record")
    expect(probe_runner_diagnostics.parse_progress("diagnostic line 7") is None,
           "and neither is ordinary probe output")

    # The other direction, which is the one that could break a shipped
    # test: `progress_lines` counts a verdict by "starts with [ and
    # contains ' <script> ... '". A progress record naming a script must
    # not match that shape.
    dispatch = probe_runner_diagnostics.format_progress(
        "begin", probe_runner_diagnostics.attempt_identity("chop", "chop_probe.py", 1, 2),
        "dispatched", elapsed=0.1, now=now)
    expect(progress_lines(dispatch, "chop_probe.py") == 0,
           f"a dispatch record naming a script is not counted as that "
           f"probe's verdict ({dispatch!r})")

    try:
        probe_runner_diagnostics.format_progress("nonsense", "x", "y", elapsed=0.0, now=now)
    except ValueError:
        expect(True, "an unknown record kind is refused at the source")
    else:
        expect(False, "an unknown record kind should be refused at the source")


def test_progress_attribution_derives_the_in_flight_set() -> None:
    print("\n-- attribution names the latest phase and every attempt "
          "started without finishing")
    now = time.time()

    def line(kind, identity, detail, elapsed):
        return probe_runner_diagnostics.format_progress(kind, identity, detail,
                                          elapsed=elapsed, now=now)

    expect(probe_runner_diagnostics.progress_attribution("") == [],
           "a capture with no progress records yields no attribution at all")
    expect(probe_runner_diagnostics.progress_attribution("just some probe output\n") == [],
           "and neither does ordinary probe output")

    alpha = probe_runner_diagnostics.attempt_identity("chop", "chop_probe.py", 1, 2)
    beta = probe_runner_diagnostics.attempt_identity("till", "till_probe.py", 1, 2)
    gamma = probe_runner_diagnostics.attempt_identity("till", "till_probe.py", 2, 2)
    capture = "\n".join([
        line("phase", "engine A", "build the scenario", 0.1),
        "some ordinary output",
        line("phase", "engine C", "load 'gen2', save 'gen3'", 300.0),
        line("phase", "cross-probes", "running 11 probe(s)", 500.0),
        line("begin", alpha, "dispatched", 500.1),
        line("begin", beta, "dispatched", 500.2),
        line("end", beta, "FAIL (10.0s)", 510.2),
        line("begin", gamma, "solo retry", 510.3),
    ]) + "\n"
    got = probe_runner_diagnostics.progress_attribution(capture)
    text = "\n".join(got)
    expect(any("cross-probes" in ln for ln in got),
           f"the LATEST phase is named, not the first (got {got!r})")
    expect("engine A" not in text and "engine C" not in text,
           f"and the superseded phases are not (got {got!r})")
    expect("+500.0s" in text,
           f"with the offset that quantifies how long it occupied "
           f"(got {got!r})")
    expect(any(alpha in ln for ln in got) and any(gamma in ln for ln in got),
           f"both attempts started without an end are named (got {got!r})")
    expect(beta not in text,
           f"and the attempt that completed is NOT reported in flight "
           f"(got {got!r})")
    expect(text.index(alpha) < text.index(gamma),
           "the in-flight attempts are listed in dispatch order")

    # A retry that finishes clears only its own attempt.
    finished = capture + line("end", gamma, "PASS (5.0s)", 515.0) + "\n"
    remaining = "\n".join(probe_runner_diagnostics.progress_attribution(finished))
    expect(alpha in remaining and gamma not in remaining,
           f"completing the retry leaves only the still-running attempt "
           f"(got {remaining!r})")


def test_progress_survives_a_forced_timeout_outside_the_ordinary_tail() -> None:
    print("\n-- a phase record emitted before a SIGKILL timeout reaches the "
          "default failure report, even buried under more than --tail lines")
    tree = Tree()
    try:
        # 40 ordinary lines after the record, against the default
        # `--tail 25`: the record is provably outside the tail, so only
        # the attribution can surface it. The probe and its engine both
        # ignore SIGTERM, so this is the real escalate-to-SIGKILL path.
        tree.add("slow", progress=(("phase", "engine C",
                                    "fresh process, load 'gen2', save 'gen3'"),),
                 tail_lines=40, hang=True, ignore_term=True,
                 engine_ignores_term=True)
        rc, out = main_with(tree, ["--timeout", "3"])
        expect(rc == 1, f"the timed-out run still fails (exit {rc})")
        expect("TIMEOUT" in out, "and is reported as a TIMEOUT")
        expect("progress: latest phase entered at" in out,
               f"the attribution line is printed:\n{out}")
        expect("engine C" in out and "load 'gen2', save 'gen3'" in out,
               f"naming the phase that was active at the kill:\n{out}")
        expect(probe_runner_diagnostics.PROGRESS_MARKER not in out,
               f"the raw record is NOT reprinted -- it fell outside the "
               f"25-line tail:\n{out}")
        expect("diagnostic line 39" in out,
               "the ordinary tail is preserved as context")
        expect("diagnostic line 0" not in out,
               f"and the complete capture is NOT dumped:\n{out}")
    finally:
        tree.cleanup()


def test_in_flight_attempts_are_derivable_from_the_default_report() -> None:
    print("\n-- a timeout names every nested attempt started without a "
          "completion, and no completed one")
    tree = Tree()
    try:
        # A probe standing in for the sweep: it enters a phase, then its
        # nested runner dispatches two attempts into the SAME pipe and
        # completes one before everything is killed. The records are the
        # real ones -- `probe_src` emits them through
        # `probe_runner_diagnostics.ProgressEmitter` -- so this is the shipped
        # convention crossing a real process boundary and a real SIGKILL.
        finished = probe_runner_diagnostics.attempt_identity("chop", "chop_probe.py", 1, 2)
        running = probe_runner_diagnostics.attempt_identity("till", "till_probe.py", 1, 2)
        retrying = probe_runner_diagnostics.attempt_identity("chop", "chop_probe.py", 2, 2)
        tree.add("nested",
                 progress=(("phase", "cross-probes", "running 2 probe(s)"),
                           ("begin", finished, "dispatched"),
                           ("begin", running, "dispatched"),
                           ("end", finished, "FAIL (1.0s)"),
                           ("begin", retrying, "solo retry")),
                 tail_lines=40, hang=True, ignore_term=True,
                 engine_ignores_term=True)
        rc, out = main_with(tree, ["--timeout", "3"])
        expect(rc == 1, f"the run fails (exit {rc})")
        expect("2 nested probe attempt(s) still in flight" in out,
               f"the in-flight count is reported:\n{out}")
        expect(running in out,
               f"the attempt still running is named ({running!r}):\n{out}")
        expect(retrying in out,
               f"and so is the solo retry in flight ({retrying!r}):\n{out}")
        expect(out.count(finished) == 0,
               f"the attempt that completed is not reported in flight:\n{out}")
        expect("cross-probes" in out,
               f"and the phase it happened in is still named:\n{out}")
        expect("diagnostic line 0" not in out,
               "without dumping the complete capture")
    finally:
        tree.cleanup()


def test_parallel_dispatch_and_retry_records_name_every_attempt() -> None:
    print("\n-- the parallel path records every attempt before it begins, "
          "and the solo retry too, without disturbing the verdict lines")
    tree = Tree()
    try:
        tree.add("alpha", dwell=0.3, descendant=False)
        tree.add("beta", exit_code=1, tail_lines=40, descendant=False,
                 progress=(("phase", "engine A", "build the scenario"),))
        rc, out = main_with(tree, ["--jobs", "2", "--retries", "1"])
        expect(rc == 1, f"the failing probe still fails the run (exit {rc})")

        pairs = record_pairs(out)
        alpha_1 = probe_runner_diagnostics.attempt_identity("alpha", "alpha_probe.py", 1, 2)
        beta_1 = probe_runner_diagnostics.attempt_identity("beta", "beta_probe.py", 1, 2)
        beta_2 = probe_runner_diagnostics.attempt_identity("beta", "beta_probe.py", 2, 2)
        for kind, identity in (("begin", alpha_1), ("end", alpha_1),
                               ("begin", beta_1), ("end", beta_1),
                               ("begin", beta_2), ("end", beta_2)):
            expect((kind, identity) in pairs,
                   f"the runner emitted a {kind} record for {identity!r} "
                   f"(got {pairs!r})")
        expect(pairs.index(("begin", beta_1)) < pairs.index(("end", beta_1))
               < pairs.index(("begin", beta_2)),
               f"the batch attempt is recorded before it begins and the "
               f"retry only after it ended (got {pairs!r})")
        expect(pairs.count(("begin", beta_1)) == 1,
               f"exactly one dispatch record per attempt (got {pairs!r})")

        # The shipped concurrency tests count verdict announcements by
        # shape; the new records must not join that count.
        expect(progress_lines(out, "alpha_probe.py") == 1
               and progress_lines(out, "beta_probe.py") == 1,
               "each probe still announces exactly one verdict")

        # Requirement 4 on the OTHER default failure path: the parallel
        # end-of-run tail block.
        block = out.split("--- beta_probe.py (FAIL) ---")[-1]
        expect("progress: latest phase entered at" in block
               and "engine A" in block,
               f"the parallel failure block carries the attribution too:\n{out}")
        expect("diagnostic line 0" not in block,
               "and still does not dump the complete capture")

        # Close the loop: the records the runner ACTUALLY printed, read
        # back by the real consumer. Dropping the completions is what a
        # kill mid-batch leaves behind, and both dispatches must then be
        # reported in flight.
        mid_batch = "\n".join(
            line for line in out.splitlines()
            if (probe_runner_diagnostics.parse_progress(line) or ProgressStub).kind != "end")
        derived = "\n".join(probe_runner_diagnostics.progress_attribution(mid_batch))
        expect(alpha_1 in derived and beta_1 in derived and beta_2 in derived,
               f"the runner's own records derive the in-flight set when the "
               f"completions are missing (got {derived!r})")
    finally:
        tree.cleanup()


# --------------------------------------------------------------------------
# Durable failure records and the retained failed check (#1982)
#
# #1768's cases above are about a probe that never finished. These are
# about one that finished and FAILED: it printed its per-check verdicts
# to a block-buffered stdout pipe and its terminal `FAIL:` summary to an
# unbuffered stderr the runner merges into that same pipe, so the
# `FAIL:` lines OVERTOOK the buffered output and landed at the top of the
# capture while `--tail 25` printed only the bottom. A real coordinated
# run spent 279.5 s to report "1 check(s) FAILED" and name the check
# nowhere.
#
# The synthetic probes below reproduce that displacement exactly -- real
# `FailureEmitter` records flushed ahead of more than `--tail` buffered
# lines -- and require the DEFAULT presentation, on both of its paths, to
# surface every one of them without dumping the capture.
# --------------------------------------------------------------------------
def failure_records(out: str) -> list[probe_runner_diagnostics.FailureRecord]:
    """Every failure record in some output, in order."""
    return [record for record in
            (probe_runner_diagnostics.parse_failure(line) for line in out.splitlines())
            if record is not None]


def test_failure_records_round_trip_and_stay_off_the_progress_channel() -> None:
    print("\n-- a failure record round-trips, and is not a progress record")
    now = time.time()
    line = probe_runner_diagnostics.format_failure(
        "check", "location_embark_probe",
        "the discovered icon never appeared at (12,7)",
        elapsed=279.4, now=now)
    record = probe_runner_diagnostics.parse_failure(line)
    expect(record is not None, f"the rendered record parses back ({line!r})")
    expect(record.kind == "check"
           and record.identity == "location_embark_probe",
           f"with its kind and identity intact (got {record!r})")
    expect(record.detail == "the discovered icon never appeared at (12,7)",
           f"and its detail intact (got {record.detail!r})")
    expect("+279.4s" in record.stamp,
           f"the stamp carries the elapsed offset, so 'at the very end of "
           f"a 279.5 s run' is readable (got {record.stamp!r})")

    awkward = probe_runner_diagnostics.format_failure(
        "setup", "probe", "no [flat] site | tried 6 seeds",
        elapsed=1.0, now=now)
    expect(probe_runner_diagnostics.parse_failure(awkward).detail
           == "no [flat] site | tried 6 seeds",
           "a detail containing the separator survives the round trip")

    # A detail spanning lines would split into a marked line and an
    # unmarked orphan the parser could only drop; one record is one line.
    multi = probe_runner_diagnostics.format_failure(
        "check", "probe", "first\nsecond\n   third", elapsed=1.0, now=now)
    expect("\n" not in multi,
           f"a multi-line detail is collapsed to one line ({multi!r})")
    expect(probe_runner_diagnostics.parse_failure(multi).detail == "first second third",
           f"keeping every word (got {probe_runner_diagnostics.parse_failure(multi)!r})")

    # The two conventions must not read each other's records: #1768's
    # promise is that a capture with no PROGRESS records yields no
    # progress attribution at all, and a failing probe emitting only
    # failure records must not break it.
    expect(probe_runner_diagnostics.parse_progress(line) is None,
           "a failure record is not a progress record")
    expect(probe_runner_diagnostics.progress_attribution(line + "\n") == [],
           "and yields no progress attribution")
    progress = probe_runner_diagnostics.format_progress("phase", "engine A", "build",
                                          elapsed=1.0, now=now)
    expect(probe_runner_diagnostics.parse_failure(progress) is None,
           "and a progress record is not a failure record")
    expect(probe_runner_diagnostics.failure_attribution(progress + "\n") == [],
           "nor does it yield failure attribution")

    expect(probe_runner_diagnostics.parse_failure("FAIL: something broke") is None,
           "an ordinary printed FAIL line is not a record")
    expect(progress_lines(line, "location_embark_probe.py") == 0,
           f"and a record naming a probe is not counted as its verdict "
           f"({line!r})")

    try:
        probe_runner_diagnostics.format_failure("nonsense", "x", "y", elapsed=0.0, now=now)
    except ValueError:
        expect(True, "an unknown record kind is refused at the source")
    else:
        expect(False, "an unknown record kind should be refused at the source")


def test_failure_attribution_names_every_recorded_failure_once() -> None:
    print("\n-- attribution names every recorded failure exactly once, "
          "keeps the two vocabularies apart, and carries the context")
    now = time.time()

    def line(kind, identity, detail, elapsed):
        return probe_runner_diagnostics.format_failure(kind, identity, detail,
                                         elapsed=elapsed, now=now)

    expect(probe_runner_diagnostics.failure_attribution("") == [],
           "a capture with no failure records yields no attribution at all")
    expect(probe_runner_diagnostics.failure_attribution("just some probe output\n") == [],
           "and neither does ordinary probe output")

    capture = "\n".join([
        line("setup", "stamp_probe", "no conforming [flat] site", 4.0),
        "some ordinary output",
        line("check", "stamp_probe", "room at (12,7) never stamped", 9.0),
        line("check", "stamp_probe", "structure.clear left the floor", 11.0),
        line("context", "engine log", "/tmp/x/engine.log", 12.0),
        line("context", "engine log tail", "vulkan: device lost", 12.0),
    ]) + "\n"
    got = probe_runner_diagnostics.failure_attribution(capture)
    text = "\n".join(got)
    expect("3 recorded failure(s)" in text,
           f"the count covers both vocabularies (got {got!r})")
    for detail in ("no conforming [flat] site",
                   "room at (12,7) never stamped",
                   "structure.clear left the floor"):
        expect(text.count(detail) == 1,
               f"{detail!r} is named exactly once (got {got!r})")
    expect("SETUP FAILURE: no conforming [flat] site" in text,
           f"a setup failure keeps its own vocabulary (got {got!r})")
    expect("FAIL: room at (12,7) never stamped" in text,
           f"and an ordinary failure keeps its own (got {got!r})")
    expect(text.index("room at (12,7)") < text.index("structure.clear"),
           "recorded failures are listed in the order they happened")
    expect("engine log: /tmp/x/engine.log" in text
           and "vulkan: device lost" in text,
           f"and the bounded context is carried beside them (got {got!r})")
    expect(text.index("structure.clear") < text.index("/tmp/x/engine.log"),
           "with the failures first and the context after them")

    # The tail is printed BESIDE the attribution, so the records
    # themselves must be withheld from it or every failure appears twice.
    stripped = probe_runner_diagnostics.without_failure_records(capture)
    expect(probe_runner_diagnostics.FAILURE_MARKER not in stripped,
           f"the records are withheld from the ordinary tail ({stripped!r})")
    expect("some ordinary output" in stripped,
           "while everything else survives it")


def test_failed_checks_survive_outside_the_ordinary_tail() -> None:
    print("\n-- a completed failing probe's failed checks reach the default "
          "report, though its records sit above the 25-line tail")
    tree = Tree()
    try:
        # The observed shape: several failure records flushed into the
        # merged pipe, then 40 block-buffered ordinary lines against the
        # default `--tail 25`. Every record is provably outside the tail,
        # so only the attribution can surface it. A phase record rides
        # along, because #1982 requirement 4 wants the failure CLASS and
        # the phase both readable without rerunning the probe.
        tree.add("stamp", exit_code=1, tail_lines=40,
                 sentinel="sentinel: the very first line of this run",
                 progress=(("phase", "engine C", "fresh process, load 'gen2'"),),
                 failures=(("setup", "stamp_probe",
                            "no conforming [flat] site in 6 seeds"),
                           ("check", "stamp_probe",
                            "room at (12,7) never stamped on first load"),
                           ("check", "stamp_probe",
                            "structure.clear did not remove the anchor floor"),
                           ("context", "engine log", "/tmp/stamp/engine.log"),
                           ("context", "engine log tail",
                            "vulkan: swapchain out of date")))
        rc, out = main_with(tree, [])
        expect(rc == 1, f"the failing run still fails (exit {rc})")
        expect("FAIL" in out, "and is reported as a FAIL")

        # Requirement 1 and 2: every failed check and its detail, named.
        expect("3 recorded failure(s)" in out,
               f"the recorded count is reported:\n{out}")
        for detail in ("no conforming [flat] site in 6 seeds",
                       "room at (12,7) never stamped on first load",
                       "structure.clear did not remove the anchor floor"):
            expect(out.count(detail) == 1,
                   f"{detail!r} is named exactly once:\n{out}")
        expect("SETUP FAILURE: no conforming [flat] site" in out,
               f"the setup vocabulary survives distinctly:\n{out}")

        # Requirement 4: the phase and the invocation context.
        expect("progress: latest phase entered at" in out and "engine C" in out,
               f"the phase the run was in is named too:\n{out}")
        expect("engine log: /tmp/stamp/engine.log" in out
               and "vulkan: swapchain out of date" in out,
               f"and the bounded engine-log context:\n{out}")

        # Requirement 6: bounded, not a dump. The sentinel is the very
        # first line of the run and stays omitted; the tail is the last
        # 25 ordinary lines and nothing more.
        expect("sentinel: the very first line" not in out,
               f"an early non-diagnostic line stays omitted:\n{out}")
        expect("diagnostic line 0" not in out,
               f"and the complete capture is NOT dumped:\n{out}")
        expect("diagnostic line 39" in out and "diagnostic line 15" in out,
               f"while the ordinary tail is preserved as context:\n{out}")
        expect("diagnostic line 14" not in out,
               f"bounded at exactly --tail lines:\n{out}")
        expect(probe_runner_diagnostics.FAILURE_MARKER not in out,
               f"and the raw records are not reprinted beside the "
               f"attribution that already carries them:\n{out}")
    finally:
        tree.cleanup()


def test_failed_checks_survive_the_parallel_presentation() -> None:
    print("\n-- the same guarantee holds in the --jobs failure block, and "
          "only the FINAL attempt's capture is the one it is read from")
    tree = Tree()
    try:
        tree.add("alpha", dwell=0.3, descendant=False)
        tree.add("beta", exit_code=1, tail_lines=40, descendant=False,
                 sentinel="sentinel: the very first line of this run",
                 progress=(("phase", "engine A", "build the scenario"),),
                 failures=(("check", "beta_probe",
                            "the overlay lost a ruin across save-load"),
                           ("check", "beta_probe",
                            "only 2/5 ruin(s) materialized after load"),
                           ("context", "engine log", "/tmp/beta/engine.log")))
        # `--retries 1` makes this the reviewer's case: `run_with_retry`
        # keeps only the FINAL attempt's capture, and the guarantee is
        # about the completed nonzero attempt that decided the verdict.
        rc, out = main_with(tree, ["--jobs", "2", "--retries", "1"])
        expect(rc == 1, f"the failing probe still fails the run (exit {rc})")

        block = out.split("--- beta_probe.py (FAIL) ---")[-1]
        expect("2 recorded failure(s)" in block,
               f"the parallel failure block carries the attribution:\n{out}")
        for detail in ("the overlay lost a ruin across save-load",
                       "only 2/5 ruin(s) materialized after load"):
            expect(block.count(detail) == 1,
                   f"{detail!r} is named exactly once in the block:\n{out}")
        expect("engine log: /tmp/beta/engine.log" in block,
               f"with its context:\n{out}")
        expect("progress: latest phase entered at" in block
               and "engine A" in block,
               f"and the phase attribution beside it:\n{out}")
        expect("sentinel: the very first line" not in block,
               f"the early non-diagnostic line stays omitted:\n{out}")
        expect("diagnostic line 0" not in block,
               f"and the complete capture is NOT dumped:\n{out}")
        expect("diagnostic line 39" in block,
               f"while the ordinary tail is preserved:\n{out}")
        expect(probe_runner_diagnostics.FAILURE_MARKER not in out,
               f"and no raw record is reprinted:\n{out}")

        # A passing probe's block does not exist at all, so nothing of
        # the mechanism reaches a green run.
        expect("alpha_probe.py (FAIL)" not in out,
               f"the passing probe gets no failure block:\n{out}")

        # The retention guarantee is about the FINAL completed nonzero
        # attempt -- the one that decided the verdict. `run_with_retry`
        # keeps only that attempt's capture, so proving the retry really
        # happened is what makes the assertions above about the second
        # attempt rather than a single-run coincidence.
        expect(len(tree.intervals("beta")) == 2,
               f"the failing probe really was retried "
               f"({tree.intervals('beta')!r})")
        expect(failure_records(block) == [],
               f"and no raw record reaches the block that already renders "
               f"them (got {failure_records(block)!r})")
    finally:
        tree.cleanup()


#: The six probes #1982 repaired. Every terminal failure any of them
#: reports must reach the runner as a durable record, so the structural
#: guard below can name them as one set.
REPAIRED_PROBES = (
    "location_embark_probe.py",
    "location_stamp_idempotent_probe.py",
    "location_content_probe.py",
    "location_overlay_probe.py",
    "portal_location_probe.py",
    "portal_ghost_probe.py",
)


def test_no_repaired_probe_still_reports_a_failure_to_stderr() -> None:
    print("\n-- no repaired probe reports a terminal failure on the "
          "unbuffered stderr the runner's tail cannot retain")

    # This is the mechanism of the whole bug, stated as a guard: the
    # runner launches each probe with `stderr=subprocess.STDOUT`, and
    # Python leaves stderr unbuffered while block-buffering the piped
    # stdout. ANY failure written to stderr therefore overtakes the
    # buffered output and lands above the retained `--tail`, whatever
    # else the probe does correctly. One such path survived the first
    # pass of this repair -- portal_ghost's phase-1 setup exit, which
    # returned without reaching `report` at all -- so the guard is over
    # the whole set rather than the paths that were noticed.
    tools = Path(TOOLS_DIR)
    for script in REPAIRED_PROBES:
        source = (tools / script).read_text(encoding="utf-8")
        expect("file=sys.stderr" not in source,
               f"{script} writes nothing to stderr; a failure there is "
               f"exactly what the runner's tail cannot keep")
        expect("FailureEmitter" in source,
               f"{script} produces durable failure records instead")


def test_a_probes_setup_exit_is_recorded_and_recoverable() -> None:
    print("\n-- portal_ghost's phase-1 setup exit records a durable setup "
          "failure the runner can recover from the complete capture")

    # The engine-free half of a needs-GPU probe: this exit is reached
    # when the fixture never materialised, BEFORE any GPU work, and it is
    # the one terminal exit that does not go through `report`. Driving it
    # directly is what proves it emits at all -- the review that found it
    # found it by reading, and nothing failed.
    import io
    import contextlib
    sys.path.insert(0, TOOLS_DIR)
    import portal_ghost_probe

    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        rc = portal_ghost_probe.report_prep_setup_failure()
    out = buf.getvalue()
    expect(rc == 1, f"the setup exit still fails the probe (got {rc})")

    records = failure_records(out)
    kinds = [record.kind for record in records]
    expect("setup" in kinds,
           f"it records a SETUP failure, not an ordinary one (got {kinds!r})")
    setup = [record for record in records if record.kind == "setup"]
    expect(len(setup) == 1,
           f"exactly one, so the runner names it once (got {setup!r})")
    expect("no ruin_small with resolvable bounds" in setup[0].detail,
           f"carrying the diagnosis (got {setup[0].detail!r})")
    expect(setup[0].identity == "portal_ghost_probe",
           f"and naming its producer (got {setup[0].identity!r})")
    expect(any(record.kind == "context" for record in records),
           f"with the prep engine log as context (got {records!r})")

    # Nothing reaches stderr on this path any more, and the runner's own
    # consumer recovers the whole thing from the capture.
    derived = "\n".join(probe_runner_diagnostics.failure_attribution(out))
    expect("SETUP FAILURE: phase 1 (headless prep)" in derived,
           f"the runner's presentation recovers it (got {derived!r})")
    expect("no ruin_small with resolvable bounds" in derived,
           f"with its detail (got {derived!r})")


#: This family's complete ordered inventory. The aggregate runs it as one
#: block at the end of the sweep, so it needs no fragments.
TESTS = (
    test_progress_records_round_trip_and_stay_out_of_the_verdict_shape,
    test_progress_attribution_derives_the_in_flight_set,
    test_progress_survives_a_forced_timeout_outside_the_ordinary_tail,
    test_in_flight_attempts_are_derivable_from_the_default_report,
    test_parallel_dispatch_and_retry_records_name_every_attempt,
    test_failure_records_round_trip_and_stay_off_the_progress_channel,
    test_failure_attribution_names_every_recorded_failure_once,
    test_failed_checks_survive_outside_the_ordinary_tail,
    test_failed_checks_survive_the_parallel_presentation,
    test_no_repaired_probe_still_reports_a_failure_to_stderr,
    test_a_probes_setup_exit_is_recorded_and_recoverable,
)
