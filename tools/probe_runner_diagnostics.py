#!/usr/bin/env python3
"""The two DURABLE RECORD protocols a probe run leaves behind.

Progress records (#1768) survive a `--timeout` SIGKILL and name the phase
a probe was in and the nested attempts still in flight. Failure records
(#1982) survive the bounded `--tail` and name every failed check and the
retained context beside it. Both are one flushed marked line per record,
formatted and parsed HERE and nowhere else, so a producer anywhere in the
tree and the aggregate runner's presentation cannot drift apart.

This is a LEAF owner (#2074): it imports nothing of the runner's, which
is what lets every probe that emits `FailureEmitter` records, and
`tools/persistence_contract_sweep.py` (which emits both kinds), take the
protocol without taking the runner.

The formats are frozen. Requirement 6 of #2074: existing probes and
nested runners must keep producing records the aggregate runner
recognizes, so the markers, separators, kind vocabularies and field
layouts below are wire format, not implementation detail.
"""
from __future__ import annotations
import time
from typing import NamedTuple

# ── Durable progress records (#1768) ──────────────────────────────────
#
# A probe's stdout is a PIPE this runner drains only when the child ends
# (`probe_runner_lifecycle.run_one`), and the child is launched as a plain
# `python3` with
# no `-u`, so an ordinary `print` sits in the child's own block buffer
# until it fills. When a slow probe is SIGKILLed at `--timeout`, that
# buffer dies with it and the artifact the operator reads names no phase
# at all.
#
# The fix is ONE convention, used by every producer that wants a record
# to survive termination, and recognized by the failure presentation at
# the other end. A progress record is a single flushed line:
#
#   #probe-progress# HH:MM:SS +12.3s | phase | engine A | build ... 'gen1'
#   #probe-progress# HH:MM:SS +0.1s  | begin | chop (chop_probe.py) attempt 1/2 | dispatched
#   #probe-progress# HH:MM:SS +45.2s | end   | chop (chop_probe.py) attempt 1/2 | PASS (45.1s)
#
# The four fields are the stamped marker, the KIND, the IDENTITY, and
# free-text detail. `begin` and `end` carry the SAME identity, which is
# what makes the in-flight set at termination derivable: every `begin`
# with no matching `end` is a nested attempt that never finished.
#
# The marker deliberately does not start with `[`: the runner's own
# verdict announcements do (`[3/12] chop_probe.py ... PASS`), and
# `probe_runner_tests.support`'s `progress_lines` helper counts those by
# that shape. Keeping the two apart means a progress record can never be
# miscounted as a verdict.
PROGRESS_MARKER = "#probe-progress#"
PROGRESS_SEP = " | "
PROGRESS_KINDS = ("phase", "begin", "end")


class ProgressRecord(NamedTuple):
    """One parsed progress record: `stamp` is `HH:MM:SS +<elapsed>s`."""
    stamp: str
    kind: str
    identity: str
    detail: str


def format_progress(kind: str, identity: str, detail: str, *,
                    elapsed: float, now: float) -> str:
    """Render one progress record in the ONE shared convention.

    Both halves of the timing evidence are carried: a wall-clock time, so
    records from two processes sharing this pipe (a sweep and the runner
    it nests) can be ordered against each other, and an offset from the
    emitting producer's own start, so how long the last named phase
    occupied before a timeout is readable without arithmetic.
    """
    if kind not in PROGRESS_KINDS:
        raise ValueError(f"unknown progress kind {kind!r}; "
                         f"expected one of {PROGRESS_KINDS}")
    stamp = f"{time.strftime('%H:%M:%S', time.localtime(now))} +{elapsed:.1f}s"
    return PROGRESS_SEP.join(
        [f"{PROGRESS_MARKER} {stamp}", kind, identity, detail])


def parse_progress(line: str) -> ProgressRecord | None:
    """One progress record, or None for any other line of child output."""
    text = line.strip()
    if not text.startswith(PROGRESS_MARKER + " "):
        return None
    fields = text.split(PROGRESS_SEP)
    if len(fields) < 4:
        return None
    stamp = fields[0][len(PROGRESS_MARKER):].strip()
    kind = fields[1].strip()
    if kind not in PROGRESS_KINDS:
        return None
    # Detail is rejoined rather than taken as fields[3]: free text may
    # itself contain the separator, and only the first three fields are
    # structural.
    return ProgressRecord(stamp, kind, fields[2].strip(),
                          PROGRESS_SEP.join(fields[3:]).strip())


def attempt_identity(key: str, script: str, attempt: int, total: int) -> str:
    """The identity a nested attempt's `begin` and `end` records share.

    Names the registered probe KEY, its script, and which attempt of how
    many this is — so the record is self-describing, and so the pairing
    is exact even when one probe is retried while another is dispatched.
    """
    return f"{key} ({script}) attempt {attempt}/{total}"


class ProgressEmitter:
    """Emits progress records against one producer's own start time.

    Every record is flushed as it is written. That is the whole point:
    the emitting process is a probe (or a runner nested inside one) whose
    stdout is a pipe nobody reads until it exits, so an unflushed record
    would die in its buffer at `--timeout` — exactly the loss #1768 is
    about.
    """

    def __init__(self, start: float | None = None) -> None:
        self.start = time.time() if start is None else start

    def emit(self, kind: str, identity: str, detail: str) -> str:
        now = time.time()
        line = format_progress(kind, identity, detail,
                               elapsed=now - self.start, now=now)
        # `file` is left at its default so a caller redirecting
        # `sys.stdout` (this suite's own drivers do) still captures it.
        print(line, flush=True)
        return line

    def phase(self, identity: str, detail: str) -> str:
        return self.emit("phase", identity, detail)

    def begin(self, identity: str, detail: str = "dispatched") -> str:
        return self.emit("begin", identity, detail)

    def end(self, identity: str, detail: str) -> str:
        return self.emit("end", identity, detail)


def progress_attribution(out: str) -> list[str]:
    """Attribution lines for a failing probe's DEFAULT presentation.

    Reads the complete captured output — which
    `probe_runner_lifecycle.run_one` holds in full —
    and returns only a short summary: the latest phase the child entered,
    and every nested attempt it started without finishing. The ordinary
    `--tail` context is printed beside this, unchanged; the complete
    capture is deliberately NOT dumped.

    A capture holding no progress records yields nothing at all, so every
    probe that emits none has exactly the failure presentation it always
    had.
    """
    records = [record for record in
               (parse_progress(line) for line in out.splitlines())
               if record is not None]
    if not records:
        return []
    lines: list[str] = []
    phases = [record for record in records if record.kind == "phase"]
    if phases:
        last = phases[-1]
        lines.append(f"progress: latest phase entered at {last.stamp}: "
                     f"{last.identity} -- {last.detail}")
    # Insertion order is dispatch order, which is the order an operator
    # wants the still-running set named in.
    started: dict[str, ProgressRecord] = {}
    for record in records:
        if record.kind == "begin":
            started[record.identity] = record
        elif record.kind == "end":
            started.pop(record.identity, None)
    if started:
        lines.append(f"progress: {len(started)} nested probe attempt(s) "
                     f"still in flight when this run ended:")
        for identity, record in started.items():
            lines.append(f"    {identity}, dispatched at {record.stamp}")
    return lines



# ── Durable failure records (#1982) ───────────────────────────────────
#
# #1768 above solves the TIMEOUT half of the same loss. This solves the
# COMPLETED-failure half, and it is a different mechanism because the
# thing lost is different.
#
# A probe writes its per-check verdicts to stdout and its terminal
# `FAIL:` summary to stderr. `probe_runner_lifecycle.run_one` merges the
# two with
# `stderr=subprocess.STDOUT`, and Python block-buffers a piped stdout
# while leaving stderr unbuffered — so the `FAIL:` lines OVERTAKE the
# stdout still sitting in the child's buffer and land near the TOP of
# the merged capture, while the default `--tail 25` prints only its
# bottom. A run that failed one check therefore reported "1 check(s)
# FAILED" and named the check nowhere. Flushing alone would fix the
# ordering but not the guarantee: with more failed checks than `--tail`
# lines, or a probe that keeps printing afterwards, the tail truncates
# them again.
#
# So a failed check is recorded the way a phase is: ONE flushed line in
# a marked convention, read back by the failure presentation from the
# COMPLETE capture. Position in the stream then stops mattering.
#
#   #probe-failure# HH:MM:SS +12.3s | check   | location_embark_probe | ...
#   #probe-failure# HH:MM:SS +12.3s | setup   | location_stamp_...    | ...
#   #probe-failure# HH:MM:SS +12.3s | context | engine log            | /tmp/...
#
# The kinds are deliberately three, not one. `check` and `setup` are the
# two vocabularies the probes already print (#1575 requirement 4: "try
# another seed" versus "there is a bug"), and losing that distinction in
# the retained output would leave an operator unable to tell a fixture
# failure from a product failure — which is the whole of #1982's
# requirement 4. `context` carries the bounded invocation evidence
# beside them: the engine log this run owned, a short tail of it, and
# what became of the artifact tree.
#
# The marker is its own, not `#probe-progress#`'s: `progress_attribution`
# reports the latest phase and the in-flight attempt set, which is a
# different question from "what failed", and its documented promise that
# a capture with no progress records yields no attribution at all stays
# exactly true.
FAILURE_MARKER = "#probe-failure#"
FAILURE_SEP = " | "
FAILURE_KINDS = ("check", "setup", "context")
# The two REPORTED kinds and the vocabulary each is printed back as. A
# kind outside this mapping is context, never a failed check.
FAILURE_LABELS = {"check": "FAIL", "setup": "SETUP FAILURE"}
# Requirement 6: a failure block stays concise. The engine-log excerpt is
# bounded here rather than at each call site, so no probe can widen it
# into a whole-capture dump by passing a large number.
FAILURE_LOG_TAIL_LINES = 10


class FailureRecord(NamedTuple):
    """One parsed failure record: `stamp` is `HH:MM:SS +<elapsed>s`."""
    stamp: str
    kind: str
    identity: str
    detail: str


def _one_line(text: object) -> str:
    """Collapse anything to a single line, so one record is one line.

    A record survives by being ONE flushed write; a detail carrying an
    embedded newline would split into a marked line and an unmarked
    orphan the parser could only drop.
    """
    return " ".join(str(text).split())


def format_failure(kind: str, identity: str, detail: str, *,
                   elapsed: float, now: float) -> str:
    """Render one failure record in the shared convention."""
    if kind not in FAILURE_KINDS:
        raise ValueError(f"unknown failure kind {kind!r}; "
                         f"expected one of {FAILURE_KINDS}")
    stamp = f"{time.strftime('%H:%M:%S', time.localtime(now))} +{elapsed:.1f}s"
    # Only the first three fields are structural, so the separator is
    # removed from the identity (field 3) and left alone in the detail.
    label = _one_line(identity).replace("|", "/") or "(unnamed)"
    return FAILURE_SEP.join(
        [f"{FAILURE_MARKER} {stamp}", kind, label, _one_line(detail)])


def parse_failure(line: str) -> FailureRecord | None:
    """One failure record, or None for any other line of child output."""
    text = line.strip()
    if not text.startswith(FAILURE_MARKER + " "):
        return None
    fields = text.split(FAILURE_SEP)
    if len(fields) < 4:
        return None
    stamp = fields[0][len(FAILURE_MARKER):].strip()
    kind = fields[1].strip()
    if kind not in FAILURE_KINDS:
        return None
    return FailureRecord(stamp, kind, fields[2].strip(),
                         FAILURE_SEP.join(fields[3:]).strip())


def failure_records(out: str) -> list[FailureRecord]:
    """Every failure record in a capture, in emission order."""
    return [record for record in
            (parse_failure(line) for line in out.splitlines())
            if record is not None]


def failure_attribution(out: str) -> list[str]:
    """The failed-check block of a failing probe's DEFAULT presentation.

    Read from the COMPLETE capture `probe_runner_lifecycle.run_one` holds,
    so a record that more
    than `--tail` lines followed is still named, and every recorded
    failure is named exactly once — the records themselves are removed
    from the tail printed beside this block (`without_failure_records`),
    so nothing here is repeated there.

    A capture holding no failure records yields nothing at all, so a
    probe that emits none has exactly the presentation it always had.
    """
    records = failure_records(out)
    if not records:
        return []
    lines: list[str] = []
    reported = [record for record in records if record.kind in FAILURE_LABELS]
    if reported:
        producers: list[str] = []
        for record in reported:
            if record.identity not in producers:
                producers.append(record.identity)
        lines.append(f"failure: {len(reported)} recorded failure(s) from "
                     f"{', '.join(producers)}:")
        for record in reported:
            lines.append(f"    [{record.stamp}] "
                         f"{FAILURE_LABELS[record.kind]}: {record.detail}")
    context = [record for record in records if record.kind == "context"]
    if context:
        lines.append("failure: retained context:")
        for record in context:
            lines.append(f"    {record.identity}: {record.detail}")
    return lines


def without_failure_records(out: str) -> str:
    """A capture with its failure records removed, for the ordinary tail.

    They are presented by `failure_attribution` above, in full; leaving
    them in the tail as well would print the same failed check twice and
    spend the tail's bounded budget on lines already shown.
    """
    return "\n".join(line for line in out.splitlines()
                     if parse_failure(line) is None)


class FailureEmitter:
    """A probe's own producer of durable failure records.

    Construct it at module scope, not inside `report()`: the elapsed
    offset each record carries is measured from this object's birth, and
    a probe's own start is what makes "+279.4s" mean "at the very end of
    a 279.5 s run".

    Every record is flushed as it is written, for the same reason
    `ProgressEmitter` flushes: this process's stdout is a pipe the runner
    drains only at exit.
    """

    def __init__(self, probe: str, *, start: float | None = None) -> None:
        self.probe = probe
        self.start = time.time() if start is None else start

    def emit(self, kind: str, identity: str, detail: str) -> str:
        now = time.time()
        line = format_failure(kind, identity, detail,
                              elapsed=now - self.start, now=now)
        # `file` is left at its default so a caller redirecting
        # `sys.stdout` still captures it.
        print(line, flush=True)
        return line

    def check(self, detail: str) -> str:
        return self.emit("check", self.probe, detail)

    def setup(self, detail: str) -> str:
        return self.emit("setup", self.probe, detail)

    def context(self, label: str, detail: str) -> str:
        return self.emit("context", label, detail)

    def report(self, failures, setup_failures=()) -> None:
        """One record per recorded failure, setup vocabulary first."""
        for failure in setup_failures:
            self.setup(failure)
        for failure in failures:
            self.check(failure)

    def context_log(self, path, *, label: str = "engine log",
                    lines: int = FAILURE_LOG_TAIL_LINES) -> None:
        """Name this run's engine log and retain a bounded tail of it.

        This is requirement 4's evidence: a fixture or infrastructure
        failure and a product failure look identical in a check name and
        different in the last few lines the engine wrote.
        """
        if not path:
            return
        self.context(label, str(path))
        try:
            with open(path, errors="replace") as handle:
                tail = handle.readlines()[-max(0, lines):]
        except OSError as error:
            self.context(f"{label} tail", f"(unreadable: {error})")
            return
        if not tail:
            self.context(f"{label} tail", "(empty)")
            return
        for line in tail:
            if line.strip():
                self.context(f"{label} tail", line.rstrip("\n"))
