#!/usr/bin/env python3
"""Show each headless lane's time against an advisory budget (#2748, CIR-18).

`.github/workflows/ci.yml`'s `headless-lanes` job runs the headless Hspec
suite as one matrix leg per lane (#2745). Every leg keeps
`--print-slow-items=20`, but that list sits at the end of the leg's raw
job log, and `tools/ci_timing_report.py` runs only on demand, so nothing
told the pull request that grew a lane. This reporter runs as the leg's
last working step and writes, into the run summary, the lane's step
duration, example count and twenty slowest items, flags a lane over its
budget, and emits a workflow warning annotation for it.

Advisory, never a gate
----------------------
The twenty-minute median run of epic #2742 is an optimization target, not
a failing gate. The report therefore:

  * exits 0 for every outcome, including a lane over budget and a lane
    whose examples failed, and turns its own runtime errors into a
    warning annotation and exit 0 -- the workflow step also carries
    `continue-on-error: true`, so neither can change `build-test`;
  * leaves the test failure to the `Headless lane` step that ran the
    tests: this tool never reads a verdict into the job's result.

`--self-test` is NOT advisory. It is a normal blocking correctness check
(CI's static-audits and `tools/ci-local.sh` both run it), and it covers
more than rendering: it re-reads the live `ci.yml`, runs the real
`Headless lane` step script against a stub `cabal`, and checks that a
failing `cabal test` still fails the step through the `tee` that captures
the log, that the start marker and log files this tool reads are
written, and that the report of that failed run still shows the lane's
duration and example count.

What is measured
----------------
The lane's headless step: the wall time from the `Mark headless lane
start` step to the end of the `Headless lane` step, that is the lane's
`cabal test` invocation, cabal's own wrapper (about 34 s at the #2743
median) included. Cache restoration and the separate compile of
`synarchy-test-headless` happen before the mark and are excluded. It is
neither the summed example durations nor the whole job time: Hspec's own
`Finished in` figure is shown beside it, so the wrapper's share is
visible. The example count comes from Hspec's footer, which
`--test-show-details=direct` puts in the log.

Data that is not there is said so
---------------------------------
A leg whose step never started, was cancelled or timed out reports a
partial or unavailable figure by name. It never invents a zero count, a
zero duration or a passing budget status, and a missing slow-item list
is reported as missing, not as an empty list. A budget verdict needs the
step to have returned AND Hspec's footer to be in the log: a run that
crashed or was killed before its summary keeps its measured duration but is
never judged, favourably or not.

Budgets
-------
`LANE_BUDGET_SECONDS` below is the one place they live. Both lanes start
from the 853 s per-lane budget of `docs/headless_suite_lane_measurement.md`
section 6 (a median successful PR run under 1200 s minus the 347 s fixed
cost of a lane job), which that report states for Hspec time at today's
median build; the measured figure here also holds cabal's wrapper, so the
comparison errs towards flagging a lane early. `--self-test` fails when a
lane the workflow's matrix names has no budget, or a budget names a lane
the matrix does not. What to do about a lane over budget is in
tools/README.md, "Headless lane time budgets".
"""

from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence

REPO_ROOT = Path(__file__).resolve().parent.parent
WORKFLOW_PATH = REPO_ROOT / ".github" / "workflows" / "ci.yml"
LANE_JOB = "headless-lanes"
LANE_STEP = "Headless lane"
START_STEP = "Mark headless lane start"
REPORT_STEP = "Report headless lane time"
LANE_STEP_ID = "headless-lane"
SCRIPT_PATH = "tools/ci_lane_report.py"

#: Advisory per-lane budgets, in seconds of the lane's headless step. The
#: one checked-in place; see the module docstring for where 853 comes from.
LANE_BUDGET_SECONDS: dict[str, int] = {
    "world": 853,
    "rest": 853,
}

#: How many of Hspec's slowest items the summary shows (the workflow asks
#: Hspec for exactly this many with `--print-slow-items=20`).
SLOW_ITEM_LIMIT = 20

#: Step outcomes (`steps.<id>.outcome`) that mean the lane's command
#: returned, so its wall time is a whole figure rather than a lower bound.
FINISHED_OUTCOMES = ("success", "failure")

#: Hspec colors the summary line (`\x1b[32m10318 examples, 0 failures\x1b[0m`)
#: even when its output is piped, and the log carries the escapes through
#: `tee`; the first CI run of this report found its footer unmatched.
_ANSI = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")
_FOOTER = re.compile(
    r"^(\d+) examples?, (\d+) failures?(?:, (\d+) pending)?\s*$")
_FINISHED = re.compile(r"^Finished in ([0-9]+(?:\.[0-9]+)?) seconds?\s*$")
_SLOW_HEADER = re.compile(r"^Slow spec items:\s*$")
_SLOW_ITEM = re.compile(r"^  (.+) \((\d+)ms\)\s*$")


@dataclass(frozen=True)
class LogFacts:
    """What the lane's Hspec output says; `None` / empty means absent."""

    examples: int | None
    failures: int | None
    pending: int | None
    hspec_seconds: float | None
    slow_section: bool
    slow_items: tuple[tuple[str, int], ...]


def parse_log(text: str) -> LogFacts:
    """Read the footer, `Finished in` figure and slow list from a log.

    Order-independent: Hspec prints the slow list to stderr and the footer
    to stdout in the same `Done` event, and the job log interleaves the two
    streams, so neither is assumed to come first. The LAST footer wins (a
    lane prints one; a stray earlier match must not shadow it).
    """
    examples = failures = pending = None
    seconds = None
    section = False
    items: list[tuple[str, int]] = []
    in_slow = False
    for raw in text.splitlines():
        line = _ANSI.sub("", raw).rstrip("\r")
        if _SLOW_HEADER.match(line):
            section = in_slow = True
            continue
        if in_slow:
            item = _SLOW_ITEM.match(line)
            if item:
                items.append((item.group(1), int(item.group(2))))
                continue
            in_slow = False
        footer = _FOOTER.match(line)
        if footer:
            examples, failures = int(footer.group(1)), int(footer.group(2))
            pending = int(footer.group(3)) if footer.group(3) else 0
            continue
        finished = _FINISHED.match(line)
        if finished:
            seconds = float(finished.group(1))
    return LogFacts(examples, failures, pending, seconds, section,
                    tuple(items))


@dataclass(frozen=True)
class LaneRecord:
    """Everything the report reads for one lane."""

    lane: str
    outcome: str            # steps.<id>.outcome; "" when none was passed
    start_epoch: int | None  # None: the start marker never ran
    now_epoch: int
    log: str | None         # None: no log file was written


def read_record(lane: str, prefix: Path, outcome: str,
                now_epoch: int) -> LaneRecord:
    def read(suffix: str) -> str | None:
        try:
            return Path(f"{prefix}{suffix}").read_text(
                encoding="utf-8", errors="replace")
        except OSError:
            return None

    start_text = read(".start")
    start = None
    if start_text is not None and start_text.strip().isdigit():
        start = int(start_text.strip())
    return LaneRecord(lane, outcome.strip(), start, now_epoch, read(".log"))


@dataclass(frozen=True)
class Assessment:
    duration: int | None     # whole seconds; None when unavailable
    complete: bool           # False: a lower bound (the step did not return)
    budget: int | None
    status: str              # over | within | not-evaluated | no-budget
    reason: str              # why a status is not-evaluated, else ""


def assess(record: LaneRecord, budgets: dict[str, int],
           facts: LogFacts | None = None) -> Assessment:
    """Judge the lane's step time against its budget.

    A budget verdict needs complete evidence: the step returned AND Hspec
    printed its footer. A `failure` outcome alone is not that -- the test
    binary can crash or be killed before it prints one -- so a footer-less
    log keeps the measured duration but is never given a favourable (or
    any) budget status. An ordinary run with failing examples prints its
    footer and is judged like any other.
    """
    budget = budgets.get(record.lane)
    if record.start_epoch is None:
        return Assessment(None, False, budget, "not-evaluated",
                          "the lane's headless step never started, so there "
                          "is no start mark")
    duration = max(0, record.now_epoch - record.start_epoch)
    complete = record.outcome in FINISHED_OUTCOMES
    if budget is None:
        return Assessment(duration, complete, None, "no-budget",
                          f"`{record.lane}` has no entry in "
                          "LANE_BUDGET_SECONDS")
    if not complete:
        return Assessment(duration, False, budget, "not-evaluated",
                          "the step did not return (outcome "
                          f"`{record.outcome or 'unknown'}`), so the figure "
                          "is a lower bound")
    if facts is None or facts.examples is None:
        return Assessment(duration, False, budget, "not-evaluated",
                          "Hspec printed no footer, so the run did not "
                          "complete (a crash or kill before the summary); "
                          "the duration is kept but is not judged")
    return Assessment(duration, True, budget,
                      "over" if duration > budget else "within", "")


def _escape_annotation(text: str) -> str:
    return (text.replace("%", "%25").replace("\r", "%0D")
            .replace("\n", "%0A"))


def _cell(text: str) -> str:
    return text.replace("|", "\\|").replace("`", "'")


def annotations(record: LaneRecord, assessment: Assessment) -> list[str]:
    """Workflow commands: a warning for a lane over budget (or unbudgeted)."""
    if assessment.status == "over":
        message = (f"Headless lane {record.lane} took {assessment.duration} s "
                   f"against its advisory budget of {assessment.budget} s "
                   f"(+{assessment.duration - assessment.budget} s). Rebalance "
                   "the lanes, add a lane, or make the expensive example "
                   "cheaper (tools/README.md, Headless lane time budgets). "
                   "This is advisory: the run's result is unchanged.")
        return [f"::warning title=Headless lane {record.lane} over its "
                f"advisory time budget::{_escape_annotation(message)}"]
    if assessment.status == "no-budget":
        message = (f"Headless lane {record.lane} has no advisory budget: add "
                   "it to LANE_BUDGET_SECONDS in tools/ci_lane_report.py.")
        return [f"::warning title=Headless lane {record.lane} has no time "
                f"budget::{_escape_annotation(message)}"]
    return []


def _seconds(value: int | None) -> str:
    return "unavailable" if value is None else f"{value} s"


def render(record: LaneRecord, assessment: Assessment,
           facts: LogFacts | None) -> str:
    """The lane's run-summary section, as GitHub-flavored Markdown."""
    lines = [f"### Headless lane `{record.lane}`", ""]
    if assessment.status == "over":
        verdict = (f"**OVER BUDGET** — {assessment.duration} s against an "
                   f"advisory budget of {assessment.budget} s "
                   f"(+{assessment.duration - assessment.budget} s)")
    elif assessment.status == "within":
        verdict = (f"within budget — {assessment.duration} s of "
                   f"{assessment.budget} s "
                   f"({assessment.budget - assessment.duration} s to spare)")
    else:
        verdict = f"budget not evaluated — {assessment.reason}"
    lines += [verdict, "", "| | |", "|---|---|"]

    duration = _seconds(assessment.duration)
    if assessment.duration is not None and not assessment.complete:
        stepped = record.outcome in FINISHED_OUTCOMES
        duration = (f"{duration} (measured; the run printed no Hspec footer)"
                    if stepped else
                    f"at least {duration} (partial: the step did not finish)")
    lines.append(f"| Step duration | {duration} |")
    lines.append(f"| Advisory budget | {_seconds(assessment.budget)} |")
    lines.append(f"| Step outcome | {_cell(record.outcome or 'unavailable')} |")
    if facts is None or facts.examples is None:
        lines.append("| Examples | unavailable: no Hspec footer in the log "
                     "(the run did not complete, or wrote no log) |")
    else:
        pending = f", {facts.pending} pending" if facts.pending else ""
        lines.append(f"| Examples | {facts.examples} "
                     f"({facts.failures} failures{pending}) |")
    if facts is None or facts.hspec_seconds is None:
        lines.append("| Hspec `Finished in` | unavailable |")
    else:
        lines.append(f"| Hspec `Finished in` | {facts.hspec_seconds} s |")
    lines.append("")

    if facts is None:
        lines.append("Slowest items: unavailable (no log was captured).")
    elif facts.slow_items:
        shown = facts.slow_items[:SLOW_ITEM_LIMIT]
        lines += [f"Slowest items ({len(shown)} shown):", "",
                  "| # | Duration | Item |", "|--:|--:|---|"]
        for rank, (label, millis) in enumerate(shown, 1):
            lines.append(f"| {rank} | {millis / 1000:.3f} s | "
                         f"{_cell(label)} |")
    elif facts.slow_section:
        lines.append("Slowest items: the list was printed but empty.")
    elif facts.examples is not None:
        lines.append("Slowest items: none reported (Hspec prints no list "
                     "when every item rounds to 0 ms).")
    else:
        lines.append("Slowest items: unavailable (the run printed no "
                     "`Slow spec items:` section and no footer, so it did "
                     "not complete).")
    lines.append("")
    return "\n".join(lines)


def record_line(record: LaneRecord, assessment: Assessment,
                facts: LogFacts | None) -> str:
    """One greppable log line, the summary's headline."""
    examples = "unavailable" if facts is None or facts.examples is None \
        else str(facts.examples)
    failures = "unavailable" if facts is None or facts.failures is None \
        else str(facts.failures)
    return (f"headless-lane {record.lane}: outcome="
            f"{record.outcome or 'unavailable'} duration="
            f"{_seconds(assessment.duration)} budget="
            f"{_seconds(assessment.budget)} status={assessment.status} "
            f"examples={examples} failures={failures}")


def report(record: LaneRecord,
           budgets: dict[str, int] = LANE_BUDGET_SECONDS
           ) -> tuple[str, list[str]]:
    """The summary Markdown and the stdout lines (record + annotations)."""
    facts = parse_log(record.log) if record.log is not None else None
    assessment = assess(record, budgets, facts)
    stdout = [record_line(record, assessment, facts)]
    stdout += annotations(record, assessment)
    return render(record, assessment, facts), stdout


def run(args: argparse.Namespace) -> int:
    now = args.now_epoch if args.now_epoch is not None else int(time.time())
    record = read_record(args.lane, Path(args.record), args.outcome, now)
    summary, stdout = report(record)
    for line in stdout:
        print(line)
    if args.github_summary:
        with open(args.github_summary, "a", encoding="utf-8") as handle:
            handle.write(summary + "\n")
    else:
        print(summary)
    return 0


# ---------------------------------------------------------------------------
# --self-test
# ---------------------------------------------------------------------------

def _slow_lines(count: int, base_ms: int = 5000) -> str:
    rows = [f"  test-headless/Foo.hs:{i}:3: /Group/example {i}/ "
            f"({base_ms - i * 100}ms)" for i in range(1, count + 1)]
    return "\n".join(rows)


PASSING_LOG = ("\nFinished in 420.9945 seconds\n10316 examples, 0 failures\n"
               "\nSlow spec items:\n" + _slow_lines(20) + "\n")
#: stderr before stdout: the job log interleaves them in either order.
REVERSED_LOG = ("\nSlow spec items:\n" + _slow_lines(20) + "\n"
                "\nFinished in 420.9945 seconds\n10316 examples, 0 failures\n")
FAILED_LOG = ("\nFailures:\n\n  test-headless/Foo.hs:9:3: \n  1) Group "
              "example 4\n       expected: 1\n        but got: 2\n\n"
              "Randomized with seed 42\n\nFinished in 301.5000 seconds\n"
              "758 examples, 3 failures, 1 pending\n"
              "\nSlow spec items:\n" + _slow_lines(5) + "\n")
PARTIAL_LOG = "\nSlow spec items:\n" + _slow_lines(2) + "\n"


def _record(lane: str, duration: int | None, outcome: str,
            log: str | None) -> LaneRecord:
    now = 1_000_000
    start = None if duration is None else now - duration
    return LaneRecord(lane, outcome, start, now, log)


def _lane_step(document: object, name: str) -> dict | None:
    jobs = document.get("jobs") if isinstance(document, dict) else None
    job = jobs.get(LANE_JOB) if isinstance(jobs, dict) else None
    for step in (job or {}).get("steps") or []:
        if isinstance(step, dict) and step.get("name") == name:
            return step
    return None


def _step_names(document: object) -> list[str]:
    jobs = document.get("jobs") if isinstance(document, dict) else {}
    job = jobs.get(LANE_JOB) if isinstance(jobs, dict) else None
    return [str(step.get("name")) for step in (job or {}).get("steps") or []
            if isinstance(step, dict)]


def check_wiring(document: object) -> list[str]:
    """What ci.yml must keep so the report is correct and stays advisory."""
    problems: list[str] = []
    where = f"{WORKFLOW_PATH.name} (job: {LANE_JOB})"
    jobs = document.get("jobs") if isinstance(document, dict) else None
    job = jobs.get(LANE_JOB) if isinstance(jobs, dict) else None
    if not isinstance(job, dict):
        return [f"{where}: no `{LANE_JOB}` job to inspect."]

    names = _step_names(document)
    for name in (START_STEP, LANE_STEP, REPORT_STEP):
        if names.count(name) != 1:
            problems.append(f"{where}: expected exactly one `{name}` step, "
                            f"found {names.count(name)}.")
    if problems:
        return problems
    if not names.index(START_STEP) < names.index(LANE_STEP) \
            < names.index(REPORT_STEP):
        problems.append(f"{where}: the steps must run in the order "
                        f"`{START_STEP}`, `{LANE_STEP}`, `{REPORT_STEP}`.")
    # The mark sits directly before the lane step, so the clock does not
    # include anything but the lane's own invocation.
    if names.index(LANE_STEP) - names.index(START_STEP) != 1:
        problems.append(f"{where}: `{START_STEP}` must be the step directly "
                        f"before `{LANE_STEP}`, so the budgeted time holds "
                        "nothing but the lane's own invocation.")

    start = _lane_step(document, START_STEP) or {}
    lane = _lane_step(document, LANE_STEP) or {}
    rep = _lane_step(document, REPORT_STEP) or {}
    guard = "steps.docs-fast-path.outputs.docs_only != 'true'"
    for label, step in (("start mark", start), ("lane", lane)):
        if "".join(str(step.get("if", "")).split()) != "".join(guard.split()):
            problems.append(f"{where}: the {label} step must carry exactly "
                            f"`if: {guard}`.")
    if lane.get("id") != LANE_STEP_ID:
        problems.append(f"{where}: `{LANE_STEP}` must carry `id: "
                        f"{LANE_STEP_ID}`, which the report reads its "
                        "outcome from.")
    if lane.get("shell") != "bash":
        problems.append(f"{where}: `{LANE_STEP}` must set `shell: bash`; its "
                        "pipefail is what keeps a failing `cabal test` "
                        "failing through the `tee` that captures the log.")
    body = str(lane.get("run") or "")
    for line in body.splitlines():
        if "cabal test synarchy-test-headless" in line and not re.search(
                r'2>&1 \| tee "\$RUNNER_TEMP/headless-lane-\$\{\{ matrix\.lane'
                r' \}\}\.log"\s*$', line):
        # every site must capture the log the report reads
            problems.append(f"{where}: `{line.strip()}` must end with "
                            "`2>&1 | tee \"$RUNNER_TEMP/headless-lane-"
                            "${{ matrix.lane }}.log\"`.")
    if "|| true" in body or "continue-on-error" in lane \
            or "set +e" in body:
        problems.append(f"{where}: `{LANE_STEP}` must stay a failing step: "
                        "no `|| true`, `set +e` or continue-on-error.")
    if "headless-lane-${{ matrix.lane }}.start" not in str(start.get("run")):
        problems.append(f"{where}: `{START_STEP}` must write "
                        "`$RUNNER_TEMP/headless-lane-${{ matrix.lane }}"
                        ".start`.")

    condition = "".join(str(rep.get("if", "")).split())
    if not condition.startswith("always()&&") or guard.replace(" ", "") \
            not in condition:
        problems.append(f"{where}: `{REPORT_STEP}` must run under "
                        f"`always() && {guard}`, so a failed lane still "
                        "reports.")
    if rep.get("continue-on-error") is not True:
        problems.append(f"{where}: `{REPORT_STEP}` must set "
                        "`continue-on-error: true`, so a reporting fault "
                        "cannot change the job's result.")
    command = str(rep.get("run") or "")
    for needle in (f"python3 {SCRIPT_PATH}", "--lane ${{ matrix.lane }}",
                   "headless-lane-${{ matrix.lane }}",
                   "${{ steps." + LANE_STEP_ID + ".outcome }}",
                   '--github-summary "$GITHUB_STEP_SUMMARY"'):
        if needle not in command:
            problems.append(f"{where}: `{REPORT_STEP}` must pass `{needle}`.")

    matrix = ((job.get("strategy") or {}).get("matrix") or {}).get("lane")
    if isinstance(matrix, list):
        for name in matrix:
            if name not in LANE_BUDGET_SECONDS:
                problems.append(f"{where}: lane `{name}` has no budget in "
                                "LANE_BUDGET_SECONDS.")
        for name in LANE_BUDGET_SECONDS:
            if name not in matrix:
                problems.append(f"LANE_BUDGET_SECONDS names `{name}`, which "
                                f"{where} does not run.")
    else:
        problems.append(f"{where}: `strategy.matrix.lane` must list lanes.")

    # The aggregate must keep deciding on the lane job's own result.
    aggregate = jobs.get("build-test") or {}
    if LANE_JOB not in (aggregate.get("needs") or []):
        problems.append(f"{WORKFLOW_PATH.name}: `build-test` must still need "
                        f"`{LANE_JOB}`.")
    return problems


def _stub_cabal(directory: Path) -> Path:
    """A `cabal` that prints a Hspec-shaped run and exits `$STUB_EXIT`."""
    bin_dir = directory / "bin"
    bin_dir.mkdir()
    stub = bin_dir / "cabal"
    stub.write_text(
        "#!/bin/sh\n"
        'echo "args: $*" >> "$STUB_RECORD"\n'
        'echo "full-tier: ${SYNARCHY_FULL_TESTS-absent}" >> "$STUB_RECORD"\n'
        'printf "%s" "$STUB_STDOUT"\n'
        'printf "%s" "$STUB_STDERR" >&2\n'
        'exit "$STUB_EXIT"\n', encoding="utf-8")
    stub.chmod(0o755)
    return bin_dir


def run_lane_step(step: dict, lane: str, worldgen: str, exit_code: int,
                  stdout: str, stderr: str) -> tuple[int, Path, str, str]:
    """Execute the workflow's own `Headless lane` script against the stub.

    Returns (exit status, record prefix, stub record, step output). The
    `${{ ... }}` expressions are substituted the way Actions would.
    """
    script = str(step.get("run") or "")
    script = script.replace("${{ matrix.lane }}", lane)
    script = script.replace("${{ steps.expensive-gates.outputs.worldgen }}",
                            worldgen)
    shell = (["bash", "--noprofile", "--norc", "-eo", "pipefail", "-c"]
             if step.get("shell") == "bash"
             else ["bash", "-e", "-c"])
    directory = Path(tempfile.mkdtemp(prefix="ci-lane-report-"))
    bin_dir = _stub_cabal(directory)
    temp = directory / "runner-temp"
    temp.mkdir()
    environment = dict(os.environ)
    environment.update({
        "PATH": f"{bin_dir}{os.pathsep}{environment.get('PATH', '')}",
        "RUNNER_TEMP": str(temp),
        "STUB_RECORD": str(directory / "stub.txt"),
        "STUB_EXIT": str(exit_code),
        "STUB_STDOUT": stdout,
        "STUB_STDERR": stderr,
    })
    environment.pop("SYNARCHY_FULL_TESTS", None)
    done = subprocess.run(shell + [script], env=environment, text=True,
                          capture_output=True, check=False)
    record = (directory / "stub.txt")
    return (done.returncode, temp / f"headless-lane-{lane}",
            record.read_text(encoding="utf-8") if record.exists() else "",
            done.stdout + done.stderr)


def _self_test() -> int:
    failures: list[str] = []

    def check(condition: bool, message: str) -> None:
        if not condition:
            failures.append(message)

    budgets = {"rest": 100}

    # 1. An in-budget lane: flagged nowhere, count and items shown.
    summary, out = report(_record("rest", 90, "success", PASSING_LOG), budgets)
    check("within budget" in summary and "OVER BUDGET" not in summary,
          "an in-budget lane is reported within budget")
    check(not any(line.startswith("::") for line in out),
          "an in-budget lane emits no annotation")
    check("| Examples | 10316 (0 failures) |" in summary,
          "the example count is shown")
    check(summary.count("\n| ") >= 20 and "example 20/" in summary
          and "| 20 |" in summary, "all twenty slow items are listed")
    check("4.900 s" in summary, "slow items carry their durations")
    check("Step duration | 90 s" in summary, "the step duration is shown")

    # 2. An over-budget lane: flagged in the summary AND by a warning.
    summary, out = report(_record("rest", 130, "success", PASSING_LOG),
                          budgets)
    check("**OVER BUDGET**" in summary and "+30 s" in summary,
          "an over-budget lane is flagged in the summary")
    warnings = [line for line in out if line.startswith("::warning ")]
    check(len(warnings) == 1 and "130 s" in warnings[0]
          and "100 s" in warnings[0], "one warning annotation names both")
    check(not any(line.startswith("::error") for line in out),
          "an over-budget lane is never an error annotation")
    exactly, _ = report(_record("rest", 100, "success", PASSING_LOG), budgets)
    check("within budget" in exactly, "a lane exactly at budget is within it")

    # 3. A lane whose examples failed still reports duration and count.
    summary, out = report(_record("rest", 80, "failure", FAILED_LOG), budgets)
    check("Step duration | 80 s" in summary
          and "758 (3 failures, 1 pending)" in summary
          and "Step outcome | failure" in summary,
          "a failed lane reports its duration, count and outcome")
    check("within budget" in summary,
          "a failed lane is still judged against its budget")
    check("Slowest items (5 shown)" in summary,
          "fewer than twenty available slow items are shown as such")

    # 4. Incomplete data is named, never invented.
    summary, out = report(_record("rest", 40, "cancelled", PARTIAL_LOG),
                          budgets)
    check("budget not evaluated" in summary and "at least 40 s" in summary,
          "a cancelled lane's duration is a labelled lower bound")
    check("Examples | unavailable" in summary,
          "a missing footer is reported as unavailable, not zero")
    check("Slowest items (2 shown)" in summary
          and "unavailable" in summary,
          "partial slow items are shown without claiming a complete list")
    check("status=not-evaluated" in out[0] and len(out) == 1,
          "a lane that never finished emits no over-budget annotation")
    summary, out = report(_record("rest", None, "skipped", None), budgets)
    check("never started" in summary and "Step duration | unavailable"
          in summary and "Slowest items: unavailable (no log" in summary,
          "a lane that never ran reports every figure as unavailable")
    summary, out = report(_record("rest", 50, "failure", "boom\n"), budgets)
    check("Examples | unavailable" in summary
          and "Slowest items: unavailable (the run printed no" in summary,
          "a log with no footer and no slow list is unavailable, not empty")
    # An aborted suite (crash or kill before the footer) keeps its measured
    # duration but earns no budget verdict, favourable or not (review of
    # #2845); a completed run with failing examples is still judged.
    check("within budget" not in summary and "OVER BUDGET" not in summary
          and "budget not evaluated" in summary
          and "Step duration | 50 s (measured; the run printed no Hspec "
              "footer)" in summary
          and "status=not-evaluated" in out[0] and len(out) == 1,
          "an aborted run without a footer is not judged within budget")
    for outcome in ("failure", "success"):
        summary, out = report(_record("rest", 500, outcome, "boom\n"),
                              budgets)
        check("OVER BUDGET" not in summary and len(out) == 1
              and "status=not-evaluated" in out[0],
              f"a footer-less {outcome} run is not judged over budget")
    summary, out = report(_record("rest", 50, "failure", FAILED_LOG), budgets)
    check("within budget" in summary and "3 failures" in summary,
          "a completed run whose examples failed is still judged")
    summary, _ = report(_record("rest", 50, "success",
                                "Finished in 1.0 seconds\n3 examples, "
                                "0 failures\n"), budgets)
    check("none reported" in summary,
          "a complete run with no slow section says none were reported")
    summary, out = report(_record("rest", 130, "cancelled", PASSING_LOG),
                          budgets)
    check("OVER BUDGET" not in summary and len(out) == 1,
          "a lower bound is never reported as over budget")

    # 5. Parsing is order-independent and takes the last footer.
    check(parse_log(PASSING_LOG) == parse_log(REVERSED_LOG),
          "stderr-before-stdout interleaving parses the same")
    facts = parse_log(FAILED_LOG)
    check((facts.examples, facts.failures, facts.pending) == (758, 3, 1)
          and facts.hspec_seconds == 301.5, "footer and Finished-in parse")
    check(parse_log("1 example, 1 failure\n").failures == 1,
          "singular footers parse")
    long_slow = parse_log("Slow spec items:\n" + _slow_lines(30) + "\n"
                          "Finished in 1.0 seconds\n1 example, 0 failures\n")
    summary, _ = report(_record("rest", 10, "success",
                                "Slow spec items:\n" + _slow_lines(30)),
                        budgets)
    check(len(long_slow.slow_items) == 30
          and "Slowest items (20 shown)" in summary
          and "example 21/" not in summary,
          "no more than twenty slow items are shown")
    check("a\\|b" in report(_record("rest", 10, "success",
                                    "Slow spec items:\n  x: /a|b/ (9ms)\n"),
                            budgets)[0], "a pipe in an item is escaped")
    check(parse_log(PASSING_LOG.replace("\n", "\r\n")).examples == 10316,
          "CRLF logs parse")
    # Hspec colors the footer even through a pipe (found on the first CI
    # run): green when clean, red when failing.
    colored = parse_log("\nFinished in 716.1051 seconds\n"
                        "\x1b[32m10318 examples, 0 failures\x1b[0m\n")
    check((colored.examples, colored.failures, colored.hspec_seconds)
          == (10318, 0, 716.1051), "a green footer parses")
    colored = parse_log("\x1b[31m758 examples, 3 failures\x1b[0m\n")
    check((colored.examples, colored.failures) == (758, 3),
          "a red footer parses")
    check(parse_log("\x1b[1mSlow spec items:\x1b[0m\n  a: /b/ (9ms)\n")
          .slow_items == (("a: /b/", 9),), "a colored slow header parses")

    # 6. An unbudgeted lane is named by a warning, not silently skipped.
    summary, out = report(_record("new", 10, "success", PASSING_LOG), budgets)
    check("no entry in LANE_BUDGET_SECONDS" in summary
          and any("has no time budget" in line for line in out),
          "a lane without a budget is called out")

    # 7. Annotation text cannot break the command.
    check(_escape_annotation("a\nb%c") == "a%0Ab%25c",
          "annotation text is escaped")

    # 8. The reporter never fails a run: not on an unreadable record,
    # not on a missing summary file, not on a lane over budget.
    with tempfile.TemporaryDirectory() as raw:
        directory = Path(raw)
        prefix = directory / "headless-lane-rest"
        Path(f"{prefix}.start").write_text("1\n", encoding="utf-8")
        Path(f"{prefix}.log").write_text(PASSING_LOG, encoding="utf-8")
        target = directory / "summary.md"
        result = subprocess.run(
            [sys.executable, str(Path(__file__).resolve()), "--lane", "rest",
             "--record", str(prefix), "--outcome", "success",
             "--github-summary", str(target), "--now-epoch", "5000"],
            text=True, capture_output=True, check=False)
        check(result.returncode == 0 and "::warning" in result.stdout,
              "an over-budget lane exits 0 with a warning annotation: "
              f"{result.returncode} {result.stdout!r} {result.stderr!r}")
        check(target.exists() and "OVER BUDGET" in target.read_text(
            encoding="utf-8"), "the summary file receives the section")
        broken = subprocess.run(
            [sys.executable, str(Path(__file__).resolve()), "--lane", "rest",
             "--record", str(prefix), "--outcome", "success",
             "--github-summary", str(directory / "no-such-dir" / "s.md")],
            text=True, capture_output=True, check=False)
        check(broken.returncode == 0
              and "::warning title=Headless lane report failed"
              in broken.stdout,
              "an unwritable summary is a warning and exit 0: "
              f"{broken.returncode} {broken.stdout!r} {broken.stderr!r}")

    # 9. The live workflow keeps the wiring, and it is mutation-checked.
    try:
        import yaml  # type: ignore
        live = yaml.safe_load(WORKFLOW_PATH.read_text(encoding="utf-8"))
    except Exception as error:  # noqa: BLE001 - reported, not raised
        failures.append(f"could not read {WORKFLOW_PATH}: {error}")
        live = None
    if live is not None:
        for problem in check_wiring(live):
            failures.append(problem)
        import copy

        def mutated(edit, expect: str) -> None:
            document = copy.deepcopy(live)
            edit(document)
            found = check_wiring(document)
            check(any(expect in problem for problem in found),
                  f"wiring check must flag: {expect}")

        def step(document: dict, name: str) -> dict:
            result = _lane_step(document, name)
            assert result is not None, name
            return result

        mutated(lambda d: step(d, LANE_STEP).pop("shell", None), "shell: bash")
        mutated(lambda d: step(d, LANE_STEP).update(
            run=step(d, LANE_STEP)["run"].replace(
                ' 2>&1 | tee "$RUNNER_TEMP/headless-lane-${{ matrix.lane }}'
                '.log"', "")), "must end with")
        mutated(lambda d: step(d, LANE_STEP).update(
            **{"continue-on-error": True}), "stay a failing step")
        mutated(lambda d: step(d, REPORT_STEP).update(
            **{"if": "steps.docs-fast-path.outputs.docs_only != 'true'"}),
            "always()")
        mutated(lambda d: step(d, REPORT_STEP).pop("continue-on-error", None),
                "continue-on-error: true")
        mutated(lambda d: step(d, REPORT_STEP).update(
            run="python3 tools/ci_lane_report.py"), "must pass")
        mutated(lambda d: step(d, LANE_STEP).pop("id", None), "id: headless-lane")
        mutated(lambda d: step(d, START_STEP).update(
            run="date +%s > /dev/null"), "must write")
        mutated(lambda d: d["jobs"][LANE_JOB]["steps"].remove(
            step(d, REPORT_STEP)), "exactly one `Report headless lane time`")
        mutated(lambda d: d["jobs"][LANE_JOB]["strategy"]["matrix"]["lane"]
                .append("extra"), "has no budget")
        mutated(lambda d: d["jobs"]["build-test"].update(needs=[]),
                "must still need")

        # 10. Failure propagation, by running the workflow's own script.
        lane_step = step(live, LANE_STEP)
        for worldgen, full in (("false", "absent"), ("true", "1")):
            code, prefix, record, output = run_lane_step(
                lane_step, "rest", worldgen, 3,
                "\nFinished in 12.0000 seconds\n40 examples, 2 failures\n",
                "\nSlow spec items:\n  Foo.hs:1:1: /a/ (7000ms)\n")
            check(code == 3,
                  "a failing `cabal test` must still fail the step "
                  f"(worldgen={worldgen}): exit {code}, {output!r}")
            check(f"full-tier: {full}" in record
                  and "--lane rest --print-slow-items=20" in record,
                  f"the lane runs once, with the right tier ({record!r})")
            check(Path(f"{prefix}.log").exists()
                  and "40 examples, 2 failures" in Path(
                      f"{prefix}.log").read_text(encoding="utf-8")
                  and "Slow spec items" in Path(
                      f"{prefix}.log").read_text(encoding="utf-8"),
                  "the log holds both of Hspec's streams")
            check("40 examples, 2 failures" in output,
                  "the lane's output still reaches the job log")
            # The start mark is its own step, so write it as that step would.
            start = step(live, START_STEP)
            check(start.get("run") is not None, "the start step has a run")
            Path(f"{prefix}.start").write_text("100\n", encoding="utf-8")
            record_in = read_record("rest", prefix, "failure", 160)
            summary, out = report(record_in, {"rest": 853})
            check("Step duration | 60 s" in summary
                  and "| Examples | 40 (2 failures) |" in summary
                  and "Foo.hs:1:1: /a/" in summary,
                  "the failed lane's summary has its duration, count and "
                  f"slow items: {summary!r}")
        code, _, _, output = run_lane_step(
            lane_step, "world", "false", 0,
            "\nFinished in 1.0000 seconds\n1 example, 0 failures\n", "")
        check(code == 0, f"a passing lane passes the step: {code} {output!r}")

        # The start step's own command writes a readable epoch.
        start_run = str(step(live, START_STEP)["run"]).replace(
            "${{ matrix.lane }}", "rest")
        with tempfile.TemporaryDirectory() as raw:
            subprocess.run(["bash", "-ec", start_run],
                           env={**os.environ, "RUNNER_TEMP": raw}, check=True)
            text = (Path(raw) / "headless-lane-rest.start").read_text(
                encoding="utf-8")
            check(text.strip().isdigit(), "the start mark is an epoch")

    if failures:
        print("ci_lane_report self-test: FAILED")
        for failure in failures:
            print(f"  - {failure}")
        return 1
    print("ci_lane_report self-test: all cases pass")
    return 0


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Show a headless lane's step time, example count and "
                    "slowest items in the run summary, against an advisory "
                    "budget (#2748). Always exits 0 outside --self-test.")
    parser.add_argument("--self-test", action="store_true",
                        help="check the report, the ci.yml wiring and the "
                             "failure propagation (blocking)")
    parser.add_argument("--lane", help="the matrix lane this leg ran")
    parser.add_argument("--record",
                        help="path prefix of the leg's .start and .log files")
    parser.add_argument("--outcome", default="",
                        help="the lane step's outcome (steps.<id>.outcome)")
    parser.add_argument("--github-summary",
                        help="append the summary here (GITHUB_STEP_SUMMARY)")
    parser.add_argument("--now-epoch", type=int,
                        help="the end of the measured step; default now")
    args = parser.parse_args(argv)
    if args.self_test:
        return _self_test()
    try:
        if not args.lane or not args.record:
            raise ValueError("--lane and --record are required")
        return run(args)
    except Exception as error:  # noqa: BLE001 - advisory: never fail the run
        message = _escape_annotation(
            f"{type(error).__name__}: {error}. The lane report is advisory; "
            "the run's result is unchanged.")
        print(f"::warning title=Headless lane report failed::{message}")
        return 0


if __name__ == "__main__":
    raise SystemExit(main())
