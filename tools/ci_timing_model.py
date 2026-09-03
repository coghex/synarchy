#!/usr/bin/env python3
"""The pure, network-free half of the CI timing report (#2277).

Everything here takes payloads that `tools/ci_timing_report.py` fetched
-- a `gh run view --json` run record, its `--json jobs` companion, and
the run's `--log` text -- and turns them into timings, diagnostics and
aggregates. Nothing in this module runs `gh`, opens a socket, or reads
the repository; that is what lets the self-test drive every rule below
from checked-in fixtures.

The timestamp endpoints, spelled out once
-----------------------------------------
Three wall times are reported, each from a NAMED pair of endpoints, so a
figure in the report can always be traced back to the API field it came
from:

  * **run**  `startedAt` -> `updatedAt`. `gh`'s `startedAt` is the REST
    payload's `run_started_at` (when the run began executing, not when
    it was queued -- that is `createdAt`), and `updatedAt` is
    `updated_at`, which for a completed run is when its last job
    finished. The queue delay `createdAt` -> `startedAt` is reported
    beside it rather than folded in.
  * **job**  `startedAt` -> `completedAt`.
  * **step** `startedAt` -> `completedAt`.

An endpoint that is absent, null, or unparseable makes that duration
UNAVAILABLE -- `None`, never `0.0`. The distinction is load-bearing: a
zero would sink a median and make a step with a broken timestamp look
like the fastest step in the run, which is the opposite of what a
diagnostic should do. `None` is excluded from every aggregate and
rendered as `unavailable` in every per-run block.

What counts as a sample
-----------------------
Only a run that is `completed` AND concluded `success` contributes to a
median or a 95th percentile. Every other conclusion (`failure`,
`cancelled`, `timed_out`, ...) and every run still `queued` or
`in_progress` is counted and listed in its own category and contributes
nothing -- a cancelled run's truncated jobs are exactly the samples that
would make the percentiles lie.

A SKIPPED step is not a sample either. GitHub gives a skipped step equal
start and completion timestamps, so it has a real, parseable, zero
duration; averaging those in would report the median `Headless test
suite` as far faster than it is on the runs where it actually ran.
Skipped jobs are excluded from the slowest-job mark for the same reason.

Steps are aggregated by `(job name, step name)`, never by step name
alone. `Toolchain`, `Resolve dependency plan` and the cache restore
steps each occur in more than one job of `.github/workflows/ci.yml`, and
collapsing them would average a probe worker's step into an audit
worker's.

The estimator
-------------
One function, `percentile`, computes both figures: R-7 linear
interpolation between the two closest ranks of the sorted sample, which
is `numpy.percentile`'s default and `statistics.quantiles(...,
method='inclusive')`'s. `median(values)` is exactly `percentile(values,
0.5)`, which reduces to the usual middle value for an odd sample and the
mean of the two middle values for an even one. A single-element sample
is that element; an empty sample is `None`, which is rendered as an
explicit empty category rather than as a zero.

Log-derived diagnostics
-----------------------
`gh run view --log` emits one line per log line, prefixed
`<job>\\t<step>\\t<timestamp> `, sometimes with a UTF-8 BOM before the
timestamp and ANSI colour inside the payload. `log_records` undoes
exactly that framing and hands the bare payload on, so the probe
protocol is read by its OWN canonical parser
(`probe_runner_diagnostics.parse_progress`) rather than by a second
copy of its wire format living here.
"""
from __future__ import annotations

import math
import os
import re
import sys
from dataclasses import dataclass, field
from datetime import datetime, timezone

# `tools/` has no `__init__.py`, so it is an implicit namespace package and
# `import tools.ci_timing_model` from the repository root leaves `tools/`
# itself OFF sys.path. The sibling imports below run at IMPORT time, so this
# has to come before them or that spelling raises ModuleNotFoundError while
# `python3 tools/ci_timing_report.py` keeps working.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_runner_diagnostics as diagnostics
from ci_cache_report import RECORD_PREFIX as CACHE_RECORD_PREFIX

#: The line `.github/workflows/ci.yml`'s `Select behavior probes` step
#: prints when its selector chose at least one probe, and the line it
#: prints instead when the selector chose none. Neither is produced by a
#: Python module this could import the constant from, so the self-test
#: greps the workflow for both: a reworded echo would otherwise silently
#: turn every probe block into "selection not recorded".
SELECTION_PREFIX = "Selected probes: "
SELECTION_EMPTY = "No behavior probes selected for this change"

#: `gh run view --log` frames every line as `<job>\t<step>\t<stamp> ...`.
LOG_FIELDS = 3
#: The leading `2026-09-02T18:29:33.5637501Z ` of a framed payload.
_LOG_STAMP = re.compile(r"^﻿?(?P<stamp>\d{4}-\d{2}-\d{2}T[0-9:.]+Z) ")
#: ANSI SGR sequences GitHub's own step echoes wrap commands in.
_ANSI = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")
#: The inverse of `probe_runner_diagnostics.attempt_identity`. That
#: function is the canonical formatter; this is the only reader of the
#: shape it produces, and the self-test round-trips the two against each
#: other so a change to the formatter fails here rather than silently
#: producing zero attributed probes.
_ATTEMPT_IDENTITY = re.compile(
    r"^(?P<key>.+?) \((?P<script>[^()]+)\) "
    r"attempt (?P<attempt>\d+)/(?P<total>\d+)$")
#: The `PASS (35.6s)` / `FAIL (1.2s)` / `TIMEOUT (900.0s)` detail
#: `probe_runner_scheduler` writes on every `end` record.
_ATTEMPT_OUTCOME = re.compile(
    r"^(?P<status>[A-Z]+)\s*\((?P<seconds>[0-9.]+)s\)")

#: A run may only contribute to an aggregate when it looks like this.
SUCCESS_STATUS = "completed"
SUCCESS_CONCLUSION = "success"
#: GitHub gives a skipped step equal endpoints; it is not a sample.
SKIPPED = "skipped"

#: The two aggregate categories, and the event each is keyed on.
#: `.github/workflows/ci.yml` triggers `push` on `master` only, so every
#: push run in the selection is a master push by construction.
CATEGORY_PULL_REQUEST = "pull_request"
CATEGORY_PUSH = "push"
CATEGORIES = (
    (CATEGORY_PULL_REQUEST, "pull-request runs"),
    (CATEGORY_PUSH, "master pushes"),
)


# ── Timestamps and durations ──────────────────────────────────────────


def parse_timestamp(text: object) -> datetime | None:
    """One GitHub timestamp, or None when it is absent or malformed."""
    if not isinstance(text, str) or not text.strip():
        return None
    candidate = text.strip()
    if candidate.endswith("Z"):
        candidate = candidate[:-1] + "+00:00"
    try:
        parsed = datetime.fromisoformat(candidate)
    except ValueError:
        return None
    if parsed.tzinfo is None:
        parsed = parsed.replace(tzinfo=timezone.utc)
    return parsed


def duration_seconds(start: object, end: object) -> float | None:
    """Seconds between two endpoints, or None if either is unavailable.

    Never 0.0 as a stand-in for "no timestamp": see the module docstring.
    A negative span (clock skew between endpoints, which GitHub does
    occasionally publish) is also unavailable rather than a negative
    sample.
    """
    first = parse_timestamp(start)
    last = parse_timestamp(end)
    if first is None or last is None:
        return None
    span = (last - first).total_seconds()
    return None if span < 0 else span


def format_duration(seconds: float | None) -> str:
    """A duration for the report, or the explicit unavailable marker."""
    if seconds is None:
        return "unavailable"
    if seconds < 60:
        return f"{seconds:.0f}s"
    return f"{seconds:.0f}s ({int(seconds) // 60}m{int(seconds) % 60:02d}s)"


# ── The timing model ──────────────────────────────────────────────────


@dataclass(frozen=True)
class StepTiming:
    """One step of one job, with its `startedAt` -> `completedAt` span."""
    job: str
    name: str
    number: int
    status: str
    conclusion: str
    started_at: str | None
    completed_at: str | None
    seconds: float | None

    @property
    def skipped(self) -> bool:
        return self.conclusion == SKIPPED

    @property
    def key(self) -> tuple[str, str]:
        """The aggregation key: step names repeat across jobs."""
        return (self.job, self.name)

    @property
    def is_sample(self) -> bool:
        return self.seconds is not None and not self.skipped


@dataclass(frozen=True)
class JobTiming:
    """One job, with its `startedAt` -> `completedAt` span and steps."""
    name: str
    status: str
    conclusion: str
    started_at: str | None
    completed_at: str | None
    seconds: float | None
    steps: tuple[StepTiming, ...]

    @property
    def skipped(self) -> bool:
        return self.conclusion == SKIPPED


@dataclass(frozen=True)
class RunTiming:
    """One workflow run: its identity, its wall time, and its jobs."""
    run_id: int
    workflow_name: str
    workflow_id: int | None
    event: str
    branch: str
    head_sha: str
    display_title: str
    status: str
    conclusion: str | None
    url: str
    created_at: str | None
    started_at: str | None
    updated_at: str | None
    seconds: float | None
    queued_seconds: float | None
    jobs: tuple[JobTiming, ...]

    @property
    def successful(self) -> bool:
        """Whether this run may contribute to a median or a percentile."""
        return (self.status == SUCCESS_STATUS
                and self.conclusion == SUCCESS_CONCLUSION)

    @property
    def category(self) -> str | None:
        """Which aggregate category this run belongs to, if any."""
        for name, _label in CATEGORIES:
            if self.event == name:
                return name
        return None

    @property
    def outcome(self) -> str:
        """The label a non-success run is counted under."""
        if self.status != SUCCESS_STATUS:
            return self.status or "unknown"
        return self.conclusion or "unknown"


def build_step(job_name: str, payload: object) -> StepTiming:
    record = payload if isinstance(payload, dict) else {}
    started = record.get("startedAt")
    completed = record.get("completedAt")
    return StepTiming(
        job=job_name,
        name=str(record.get("name", "")),
        number=int(record.get("number") or 0),
        status=str(record.get("status") or ""),
        conclusion=str(record.get("conclusion") or ""),
        started_at=started if isinstance(started, str) else None,
        completed_at=completed if isinstance(completed, str) else None,
        seconds=duration_seconds(started, completed),
    )


def build_job(payload: object) -> JobTiming:
    record = payload if isinstance(payload, dict) else {}
    name = str(record.get("name", ""))
    started = record.get("startedAt")
    completed = record.get("completedAt")
    steps = record.get("steps")
    return JobTiming(
        name=name,
        status=str(record.get("status") or ""),
        conclusion=str(record.get("conclusion") or ""),
        started_at=started if isinstance(started, str) else None,
        completed_at=completed if isinstance(completed, str) else None,
        seconds=duration_seconds(started, completed),
        steps=tuple(build_step(name, step)
                    for step in (steps if isinstance(steps, list) else [])),
    )


def build_run(meta: object, jobs_payload: object) -> RunTiming:
    """A `RunTiming` from `gh run view --json ...` and `--json jobs`."""
    record = meta if isinstance(meta, dict) else {}
    jobs = jobs_payload if isinstance(jobs_payload, list) else []
    workflow_id = record.get("workflowDatabaseId")
    started = record.get("startedAt")
    updated = record.get("updatedAt")
    created = record.get("createdAt")
    return RunTiming(
        run_id=int(record.get("databaseId") or 0),
        workflow_name=str(record.get("workflowName") or ""),
        workflow_id=int(workflow_id) if isinstance(workflow_id, int) else None,
        event=str(record.get("event") or ""),
        branch=str(record.get("headBranch") or ""),
        head_sha=str(record.get("headSha") or ""),
        display_title=str(record.get("displayTitle") or ""),
        status=str(record.get("status") or ""),
        conclusion=(record.get("conclusion") or None),
        url=str(record.get("url") or ""),
        created_at=created if isinstance(created, str) else None,
        started_at=started if isinstance(started, str) else None,
        updated_at=updated if isinstance(updated, str) else None,
        seconds=duration_seconds(started, updated),
        queued_seconds=duration_seconds(created, started),
        jobs=tuple(build_job(job) for job in jobs),
    )


def slowest_jobs(run: RunTiming) -> frozenset[str]:
    """The longest-duration non-skipped job(s), ties included.

    Deliberately NOT called a critical path. `gh run view --json jobs`
    publishes no dependency edges, and `.github/workflows/ci.yml` has a
    real prerequisite/fan-out/fan-in graph, so timestamps alone cannot
    reconstruct which job actually gated the run. The longest job is a
    fact this data supports; a critical path is not.
    """
    timed = [job for job in run.jobs
             if job.seconds is not None and not job.skipped]
    if not timed:
        return frozenset()
    longest = max(job.seconds for job in timed)
    return frozenset(job.name for job in timed if job.seconds == longest)


def slow_steps(run: RunTiming, threshold: float) -> list[StepTiming]:
    """Every non-skipped, timed step at or above `threshold` seconds.

    Ordered longest first, which is the order the question "where did
    the run go" wants them in.
    """
    candidates = [step for job in run.jobs for step in job.steps
                  if step.is_sample and step.seconds >= threshold]
    return sorted(candidates, key=lambda step: (-(step.seconds or 0.0),
                                                step.job, step.number))


# ── Log framing ───────────────────────────────────────────────────────


@dataclass(frozen=True)
class LogRecord:
    """One `gh run view --log` line with its framing removed."""
    job: str
    step: str
    stamp: str
    payload: str


def log_records(text: str):
    """Every framed log line, with the prefix, BOM and ANSI stripped.

    A line that does not carry the three-field framing is dropped: it
    cannot be attributed to a job or a step, and every marker this
    module reads is emitted through a step's own stdout and therefore
    always framed.
    """
    for line in text.splitlines():
        fields = line.split("\t", LOG_FIELDS - 1)
        if len(fields) < LOG_FIELDS:
            continue
        job, step, rest = fields
        match = _LOG_STAMP.match(rest)
        if match is None:
            continue
        payload = _ANSI.sub("", rest[match.end():]).rstrip()
        yield LogRecord(job.strip(), step.strip(),
                        match.group("stamp"), payload)


# ── Probe diagnostics ─────────────────────────────────────────────────


@dataclass(frozen=True)
class ProbeAttempt:
    """One `begin`/`end` pair from the probe progress protocol."""
    key: str
    script: str
    attempt: int
    total: int
    begin_stamp: str
    end_stamp: str | None
    status: str | None
    seconds: float | None

    @property
    def complete(self) -> bool:
        """Whether this attempt's `begin` was ever matched by an `end`."""
        return self.end_stamp is not None


@dataclass(frozen=True)
class ProbeSummary:
    """Every attempt observed for one probe key, in dispatch order."""
    key: str
    script: str
    attempts: tuple[ProbeAttempt, ...]

    @property
    def attempt_count(self) -> int:
        """Attempts actually OBSERVED, not the `n/N` the identity names.

        The identity's denominator is the retry budget the run was
        configured with; what a report wants is how many attempts this
        probe really consumed.
        """
        return len(self.attempts)

    @property
    def budget(self) -> int:
        return max((attempt.total for attempt in self.attempts), default=0)

    @property
    def consumed_seconds(self) -> float | None:
        """Total duration across every COMPLETED attempt."""
        timed = [attempt.seconds for attempt in self.attempts
                 if attempt.seconds is not None]
        return sum(timed) if timed else None

    @property
    def final_status(self) -> str | None:
        for attempt in reversed(self.attempts):
            if attempt.status is not None:
                return attempt.status
        return None

    @property
    def incomplete(self) -> tuple[ProbeAttempt, ...]:
        """Attempts whose `begin` never got an `end`.

        The protocol's own promise: a `begin` with no matching `end` is
        an attempt that never finished -- the probe was still running
        when the runner was killed, or the log was truncated.
        """
        return tuple(attempt for attempt in self.attempts
                     if not attempt.complete)


def parse_attempt_identity(identity: str) -> tuple[str, str, int, int] | None:
    """`(key, script, attempt, total)` from one attempt identity.

    The inverse of `probe_runner_diagnostics.attempt_identity`, whose
    format is that module's to own. The self-test round-trips real
    identities through both directions rather than restating the shape.
    """
    match = _ATTEMPT_IDENTITY.match(identity.strip())
    if match is None:
        return None
    return (match.group("key"), match.group("script"),
            int(match.group("attempt")), int(match.group("total")))


def parse_attempt_outcome(detail: str) -> tuple[str | None, float | None]:
    """`(status, seconds)` from an `end` record's `PASS (35.6s)` detail."""
    match = _ATTEMPT_OUTCOME.match(detail.strip())
    if match is None:
        return (None, None)
    return (match.group("status"), float(match.group("seconds")))


def probe_summaries(records) -> tuple[ProbeSummary, ...]:
    """Every probe attempt in a log, keyed by probe, in dispatch order.

    Pairing is by the identity `begin` and `end` share, exactly as
    `probe_runner_diagnostics` defines it, so a retry pairs with its own
    retry rather than with the batch attempt that preceded it.
    """
    order: list[str] = []
    scripts: dict[str, str] = {}
    attempts: dict[str, list[dict]] = {}
    open_attempts: dict[str, dict] = {}
    for record in records:
        progress = diagnostics.parse_progress(record.payload)
        if progress is None or progress.kind not in ("begin", "end"):
            continue
        parsed = parse_attempt_identity(progress.identity)
        if parsed is None:
            continue
        key, script, attempt, total = parsed
        if key not in attempts:
            order.append(key)
            attempts[key] = []
            scripts[key] = script
        if progress.kind == "begin":
            pending = {"key": key, "script": script, "attempt": attempt,
                       "total": total, "begin_stamp": progress.stamp,
                       "end_stamp": None, "status": None, "seconds": None}
            attempts[key].append(pending)
            open_attempts[progress.identity] = pending
            continue
        pending = open_attempts.pop(progress.identity, None)
        if pending is None:
            # An `end` with no `begin`: the log was truncated at the
            # front, or two runners shared this stream. Record it as a
            # complete attempt of its own rather than dropping the only
            # duration evidence it carries.
            pending = {"key": key, "script": script, "attempt": attempt,
                       "total": total, "begin_stamp": progress.stamp,
                       "end_stamp": None, "status": None, "seconds": None}
            attempts[key].append(pending)
        status, seconds = parse_attempt_outcome(progress.detail)
        pending["end_stamp"] = progress.stamp
        pending["status"] = status
        pending["seconds"] = seconds
    return tuple(
        ProbeSummary(
            key=key,
            script=scripts[key],
            attempts=tuple(
                ProbeAttempt(
                    key=pending["key"], script=pending["script"],
                    attempt=pending["attempt"], total=pending["total"],
                    begin_stamp=pending["begin_stamp"],
                    end_stamp=pending["end_stamp"],
                    status=pending["status"], seconds=pending["seconds"])
                for pending in sorted(attempts[key],
                                      key=lambda item: item["attempt"])))
        for key in order)


@dataclass(frozen=True)
class LogDiagnostics:
    """Everything the report reads out of one run's log text."""
    available: bool
    reason: str | None = None
    selection: tuple[str, ...] | None = None
    selection_empty: bool = False
    probes: tuple[ProbeSummary, ...] = ()
    cache_records: tuple[str, ...] = ()

    @classmethod
    def unavailable(cls, reason: str) -> "LogDiagnostics":
        """The diagnostics of a run whose log could not be read.

        A cancelled or still-running run has no downloadable log, and an
        old one's has expired. That must not abort the batch or the run
        block: the run's metadata and timings are still real, and the
        two log-derived sections say so instead of appearing empty.
        """
        return cls(available=False, reason=reason)


def read_log(text: str) -> LogDiagnostics:
    """Parse one run's complete `gh run view --log` output."""
    records = list(log_records(text))
    selection: tuple[str, ...] | None = None
    selection_empty = False
    cache: list[str] = []
    for record in records:
        payload = record.payload
        if payload.startswith(SELECTION_PREFIX):
            names = [name.strip() for name
                     in payload[len(SELECTION_PREFIX):].split(",")]
            selection = tuple(name for name in names if name)
        elif payload.startswith(SELECTION_EMPTY):
            selection_empty = True
        elif payload.startswith(CACHE_RECORD_PREFIX + " "):
            cache.append(payload)
    return LogDiagnostics(
        available=True,
        selection=selection,
        selection_empty=selection_empty,
        probes=probe_summaries(records),
        cache_records=tuple(cache),
    )


# ── Aggregates ────────────────────────────────────────────────────────


def percentile(values, quantile: float) -> float | None:
    """R-7 linear interpolation between the two closest ranks.

    The same estimator `numpy.percentile` and
    `statistics.quantiles(..., method='inclusive')` use, so a figure
    here can be checked against either without a conversion. An empty
    sample has no percentile and returns None -- never 0.0, which would
    render as a real, very fast measurement.
    """
    ordered = sorted(value for value in values if value is not None)
    if not ordered:
        return None
    if len(ordered) == 1:
        return float(ordered[0])
    position = (len(ordered) - 1) * quantile
    low = math.floor(position)
    high = math.ceil(position)
    if low == high:
        return float(ordered[low])
    return float(ordered[low]
                 + (ordered[high] - ordered[low]) * (position - low))


def median(values) -> float | None:
    """The 50th percentile under the one estimator above."""
    return percentile(values, 0.5)


@dataclass
class Aggregate:
    """Median/P95 over one category's successful runs."""
    category: str
    label: str
    runs: list[RunTiming] = field(default_factory=list)
    excluded: list[RunTiming] = field(default_factory=list)

    @property
    def sample_count(self) -> int:
        return len(self.runs)

    def run_seconds(self) -> list[float]:
        return [run.seconds for run in self.runs if run.seconds is not None]

    def step_samples(self) -> dict[tuple[str, str], list[float]]:
        samples: dict[tuple[str, str], list[float]] = {}
        for run in self.runs:
            for job in run.jobs:
                for step in job.steps:
                    if step.is_sample:
                        samples.setdefault(step.key, []).append(step.seconds)
        return samples


def aggregate(runs) -> tuple[list[Aggregate], list[RunTiming]]:
    """Split runs into the two categories, plus everything uncategorised.

    Within a category, `runs` holds only the successful samples and
    `excluded` holds every non-success run in that category, so the
    report can print both the percentiles and the reason a category has
    fewer samples than runs.
    """
    aggregates = [Aggregate(name, label) for name, label in CATEGORIES]
    by_category = {block.category: block for block in aggregates}
    other: list[RunTiming] = []
    for run in runs:
        block = by_category.get(run.category or "")
        if block is None:
            other.append(run)
        elif run.successful:
            block.runs.append(run)
        else:
            block.excluded.append(run)
    return aggregates, other
