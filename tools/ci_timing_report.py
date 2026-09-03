#!/usr/bin/env python3
"""Report CI lane and step timings for one run or the last N (#2277).

`docs/ci_runtime_reduction_design.md` CIR-1 asks for durable timing and
selection diagnostics per run. Its cache half landed as #1358
(`tools/ci_cache_report.py`); this is its on-demand historical-reporting
and Hspec-diagnostic portion. CIR-1 also asks for per-run workflow
summaries and selected expensive-gate diagnostics, which this command
deliberately does NOT add: it introduces no CI step and no secret, and
runs from a developer machine on the `gh` authentication already there.

Why it exists
-------------
Every "on the PR's own CI run" acceptance line in #2272-#2275 was
checked by hand: `gh run view <id> --json jobs`, a `jq` pass over step
timestamps, a `grep` of the raw log for `#probe-progress#` and the cache
records. The 2026-09-02 measurement that produced those four issues took
a dozen ad-hoc commands and was not repeatable by the next agent without
redoing them. This is that measurement as one command.

What it prints
--------------
Per run: the event, the branch (or the pull request, when it can be
resolved), the conclusion, the run's wall time, every job's wall time
with the SLOWEST job marked, every step at or above `--threshold`
seconds, the behavior-probe selection with each probe's observed
attempts and durations, and every `CI_CACHE_REPORT` record the run
emitted.

Across runs: median and 95th-percentile wall time for the run and for
each `(job, step)` pair, computed separately for pull-request runs and
master pushes, from SUCCESSFUL runs only. Every other conclusion, and
every run still queued or in progress, is listed and counted in its own
category and contributes to no percentile.

"Slowest job", not "critical path"
----------------------------------
The marked job is the longest-duration non-skipped job, with ties all
marked. `.github/workflows/ci.yml` really does have a
prerequisite/fan-out/fan-in graph, but `gh run view --json jobs`
publishes no dependency edges, so timestamps alone cannot reconstruct
which job gated the run. The longest job is what this data supports.

Run selection
-------------
`--last N` selects runs of the `CI` workflow -- resolved by its file
path `.github/workflows/ci.yml`, not by its display name -- before any
event or branch filter is applied. The repository also runs
`review-gate` and `ntfy-notify` on pull requests, so an event filter
alone would mix three workflows' timings into one table. An explicit
`--run` naming a run of any other workflow fails with that workflow's
name rather than reporting it.

Degrading rather than aborting
------------------------------
A cancelled, expired or still-running run has no downloadable log. Its
metadata and its job/step timings are still real, so the run block is
printed with its probe and cache sections marked unavailable and the
batch continues.

Usage:
  python3 tools/ci_timing_report.py --run 33666483367
  python3 tools/ci_timing_report.py --last 10
  python3 tools/ci_timing_report.py --last 20 --event pull_request
  python3 tools/ci_timing_report.py --last 10 --branch master --threshold 60
  python3 tools/ci_timing_report.py --self-test
Exit codes: 0 = the report printed (or every self-test check passed),
1 = a `gh` call, a selection, or a self-test check failed.

Where each layer lives
----------------------
  tools/ci_timing_model.py       every pure rule: timestamp endpoints,
                                 the timing model, log framing, the
                                 probe and cache diagnostics, the
                                 estimator and the aggregates. Imports
                                 no network and no repository path.
  tools/ci_timing_report.py      this facade: `gh`, the CLI, rendering.
  tools/test_ci_timing_report.py every fixture-driven check, reached
                                 only through `--self-test`.

This command is deliberately absent from `.github/workflows/ci.yml` and
`tools/ci-local.sh`. It is an on-demand diagnostic, not a gate: CIR-1
forbids turning timing bands into a failing check without a maintainer
decision. `tools/ci_parity_audit.py` therefore needs no exemption entry
for it.
"""
from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys

# `tools/` has no `__init__.py`, so it is an implicit namespace package and
# `import tools.ci_timing_report` from the repository root leaves `tools/`
# itself OFF sys.path. The sibling import below runs at IMPORT time, so this
# has to come before it or that spelling raises ModuleNotFoundError while
# `python3 tools/ci_timing_report.py` keeps working.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ci_timing_model as model

#: The workflow this report is about, identified by PATH. `gh` will match
#: `--workflow` against a display name too, but names are not unique and
#: this repository's other pull-request workflows would then be selected
#: by an event filter alone.
CI_WORKFLOW_PATH = ".github/workflows/ci.yml"
#: Steps shorter than this are omitted from a run block by default.
DEFAULT_THRESHOLD = 30.0
#: The `gh run view` / `gh run list` fields the model consumes.
RUN_FIELDS = ("attempt,conclusion,createdAt,databaseId,displayTitle,event,"
              "headBranch,headSha,number,startedAt,status,updatedAt,url,"
              "workflowDatabaseId,workflowName")


class ReportError(Exception):
    """A failure that should end the command with a named reason."""


# ── `gh` ──────────────────────────────────────────────────────────────


def run_gh(args: list[str]) -> str:
    """One `gh` call, returning stdout.

    A non-zero exit raises `ReportError` carrying `gh`'s own stderr. The
    CALLER decides what that means: `fetch_log` catches it and degrades
    to an unavailable-log block, while a failed run lookup ends the
    command.
    """
    if shutil.which("gh") is None:
        raise ReportError("`gh` is not on PATH; this report reads the CI "
                          "history through the GitHub CLI you already "
                          "authenticate with.")
    completed = subprocess.run(["gh", *args], capture_output=True, text=True)
    if completed.returncode != 0:
        detail = (completed.stderr or completed.stdout or "").strip()
        raise ReportError(f"`gh {' '.join(args)}` failed: {detail}")
    return completed.stdout


def gh_json(args: list[str]):
    text = run_gh(args)
    try:
        return json.loads(text)
    except json.JSONDecodeError as error:
        raise ReportError(f"`gh {' '.join(args)}` did not return JSON "
                          f"({error}).") from error


def resolve_repo() -> str:
    payload = gh_json(["repo", "view", "--json", "nameWithOwner"])
    name = payload.get("nameWithOwner") if isinstance(payload, dict) else None
    if not name:
        raise ReportError("could not resolve the repository; pass --repo "
                          "OWNER/NAME.")
    return str(name)


def resolve_ci_workflow(repo: str) -> tuple[int, str]:
    """The `(id, name)` of the workflow at `CI_WORKFLOW_PATH`."""
    payload = gh_json(["workflow", "list", "-R", repo, "--all",
                       "--json", "id,name,path"])
    for entry in payload if isinstance(payload, list) else []:
        if isinstance(entry, dict) and entry.get("path") == CI_WORKFLOW_PATH:
            return (int(entry["id"]), str(entry.get("name") or ""))
    raise ReportError(f"{repo} has no workflow at {CI_WORKFLOW_PATH}; this "
                      "report is about that workflow specifically.")


def fetch_run(repo: str, run_id: int) -> model.RunTiming:
    meta = gh_json(["run", "view", str(run_id), "-R", repo,
                    "--json", RUN_FIELDS])
    jobs = gh_json(["run", "view", str(run_id), "-R", repo, "--json", "jobs"])
    payload = jobs.get("jobs") if isinstance(jobs, dict) else None
    return model.build_run(meta, payload)


def fetch_runs(repo: str, workflow_id: int, limit: int,
               event: str | None, branch: str | None) -> list[model.RunTiming]:
    args = ["run", "list", "-R", repo, "--workflow", str(workflow_id),
            "--limit", str(limit), "--json", RUN_FIELDS]
    if event:
        args += ["--event", event]
    if branch:
        args += ["--branch", branch]
    listed = gh_json(args)
    runs: list[model.RunTiming] = []
    for entry in listed if isinstance(listed, list) else []:
        run_id = entry.get("databaseId") if isinstance(entry, dict) else None
        if not run_id:
            continue
        try:
            jobs = gh_json(["run", "view", str(run_id), "-R", repo,
                            "--json", "jobs"])
        except ReportError as error:
            # One unreadable run must not cost the other nine. Its
            # metadata is already in hand, so it reports with no job
            # rows rather than aborting the batch.
            print(f"ci_timing_report.py: run {run_id}'s jobs are "
                  f"unavailable ({error}); reporting its metadata only.")
            jobs = None
        payload = jobs.get("jobs") if isinstance(jobs, dict) else None
        runs.append(model.build_run(entry, payload))
    return runs


def fetch_log(repo: str, run: model.RunTiming) -> model.LogDiagnostics:
    """One run's log, or an explicit unavailable with its reason.

    A run that is not completed has no archive to download, and an
    expired or purged one answers with `gh`'s own error. Either way the
    batch continues: the run's timings do not depend on its log.
    """
    if run.status != model.SUCCESS_STATUS:
        return model.LogDiagnostics.unavailable(
            f"run is {run.status or 'not completed'}; GitHub publishes no "
            "downloadable log until a run finishes")
    try:
        text = run_gh(["run", "view", str(run.run_id), "-R", repo, "--log"])
    except ReportError as error:
        return model.LogDiagnostics.unavailable(str(error))
    return model.read_log(text)


def resolve_pull_request(repo: str, run: model.RunTiming,
                         cache: dict[str, int | None]) -> int | None:
    """The pull request a `pull_request` run belongs to, if resolvable.

    The run payload's own `pull_requests` array is empty for most runs,
    so this asks which pull requests contain the head SHA. Best effort
    by design: a deleted branch, a fork, or a rate limit yields None and
    the run is identified by its branch instead.
    """
    if run.event != model.CATEGORY_PULL_REQUEST or not run.head_sha:
        return None
    if run.head_sha in cache:
        return cache[run.head_sha]
    number: int | None = None
    try:
        payload = gh_json(["api", f"repos/{repo}/commits/{run.head_sha}/pulls",
                           "--jq", "[.[] | {number, head: .head.ref}]"])
    except ReportError:
        payload = None
    if isinstance(payload, list) and payload:
        # A commit can sit on more than one pull request; the run's own
        # head branch is what says which of them this run belongs to.
        matching = [entry for entry in payload
                    if isinstance(entry, dict)
                    and entry.get("head") == run.branch]
        chosen = (matching or payload)[0]
        if isinstance(chosen, dict) and isinstance(chosen.get("number"), int):
            number = chosen["number"]
    cache[run.head_sha] = number
    return number


# ── Rendering ─────────────────────────────────────────────────────────


def workflow_mismatch(run: model.RunTiming, workflow_id: int,
                      workflow_name: str) -> str | None:
    """Why this run is not reportable, or None when it is.

    A run whose payload names a DIFFERENT workflow is refused outright:
    `--run` is the one selection path that bypasses `gh run list
    --workflow`, and this repository's `review-gate` and `ntfy-notify`
    workflows also run on pull requests, so silently reporting one of
    theirs would put another workflow's lanes in a CI table. A run whose
    payload names no workflow at all is reported: nothing contradicts
    the selection, and refusing would break on a payload shape rather
    than on a real mismatch.
    """
    if run.workflow_id is None or run.workflow_id == workflow_id:
        return None
    return (f"run {run.run_id} belongs to the "
            f"{run.workflow_name or 'unknown'!r} workflow, not to "
            f"{workflow_name!r} ({CI_WORKFLOW_PATH}). This report is about "
            "that workflow's lanes and steps; re-run it with a run of that "
            "workflow.")


def describe_identity(run: model.RunTiming, pull_request: int | None) -> str:
    if pull_request is not None:
        return f"PR #{pull_request} (head {run.branch})"
    return f"branch {run.branch}" if run.branch else "(no branch)"


def render_run(run: model.RunTiming, diagnostics: model.LogDiagnostics,
               pull_request: int | None, threshold: float) -> list[str]:
    lines = [f"== run {run.run_id} -- {run.workflow_name or '(unnamed)'}"]
    lines.append(f"   url          {run.url}")
    lines.append(f"   event        {run.event or '(unknown)'}  "
                 f"{describe_identity(run, pull_request)}")
    if run.display_title:
        lines.append(f"   title        {run.display_title}")
    lines.append(f"   conclusion   {run.outcome}")
    # `updatedAt` is the last write to the run record, which for a
    # FINISHED run is when its last job ended and for an unfinished one
    # is only "as of now" -- so the label says which of the two this is
    # rather than presenting a partial run as a measured one.
    label = ("wall time   " if run.status == model.SUCCESS_STATUS
             else "elapsed     ")
    lines.append(f"   {label} "
                 f"{model.format_duration(run.seconds)}   "
                 f"[startedAt {run.started_at or 'unavailable'} -> "
                 f"updatedAt {run.updated_at or 'unavailable'}"
                 f"{'' if run.status == model.SUCCESS_STATUS else ', so far'}]")
    lines.append(f"   queued for   "
                 f"{model.format_duration(run.queued_seconds)}   "
                 f"[createdAt -> startedAt]")

    slowest = model.slowest_jobs(run)
    lines.append("   jobs [startedAt -> completedAt]")
    if not run.jobs:
        lines.append("       (no jobs reported)")
    for job in sorted(run.jobs,
                      key=lambda item: (-(item.seconds or -1.0), item.name)):
        mark = "  <- slowest job" if job.name in slowest else ""
        note = "  (skipped)" if job.skipped else ""
        lines.append(f"       {model.format_duration(job.seconds):>16}  "
                     f"{job.name}{note}{mark}")
    if len(slowest) > 1:
        lines.append(f"       (tie: {len(slowest)} jobs share the longest "
                     "duration)")

    steps = model.slow_steps(run, threshold)
    lines.append(f"   steps at or above {threshold:.0f}s "
                 "[startedAt -> completedAt]")
    if not steps:
        lines.append(f"       (no step reached {threshold:.0f}s)")
    for step in steps:
        lines.append(f"       {model.format_duration(step.seconds):>16}  "
                     f"{step.job} / {step.name}")
    unavailable = [step for job in run.jobs for step in job.steps
                   if step.seconds is None and not step.skipped]
    for step in unavailable:
        lines.append(f"       {'unavailable':>16}  {step.job} / {step.name}  "
                     "(missing timestamp; excluded from aggregates)")

    lines.extend(render_probes(diagnostics))
    lines.extend(render_cache(diagnostics))
    return lines


def render_probes(diagnostics: model.LogDiagnostics) -> list[str]:
    lines = ["   behavior probes"]
    if not diagnostics.available:
        lines.append(f"       unavailable: {diagnostics.reason}")
        return lines
    if diagnostics.selection_empty:
        lines.append("       selection: none (the selector chose no probe "
                     "for this change)")
    elif diagnostics.selection is None:
        lines.append("       selection: not recorded in this log "
                     "(no probe job ran)")
    else:
        lines.append(f"       selection: {len(diagnostics.selection)} probe(s)"
                     f" -- {', '.join(diagnostics.selection)}")
    if not diagnostics.probes:
        lines.append("       (no #probe-progress# records in this log)")
        return lines
    for summary in sorted(diagnostics.probes,
                          key=lambda item: -(item.consumed_seconds or 0.0)):
        attempts = summary.attempt_count
        plural = "" if attempts == 1 else "s"
        lines.append(
            f"       {model.format_duration(summary.consumed_seconds):>16}  "
            f"{summary.key} ({summary.script})  {attempts} attempt{plural}"
            f" of a budget of {summary.budget}  "
            f"{summary.final_status or 'no outcome recorded'}")
        if attempts > 1 or summary.incomplete:
            for attempt in summary.attempts:
                outcome = (f"{attempt.status} "
                           f"({model.format_duration(attempt.seconds)})"
                           if attempt.complete
                           else "INCOMPLETE (begin with no matching end)")
                lines.append(f"           attempt {attempt.attempt}/"
                             f"{attempt.total}  {outcome}")
    selected = set(diagnostics.selection or ())
    observed = {summary.key for summary in diagnostics.probes}
    missing = sorted(selected - observed)
    if missing:
        lines.append(f"       selected but no records: {', '.join(missing)}")
    return lines


def render_cache(diagnostics: model.LogDiagnostics) -> list[str]:
    lines = ["   cache"]
    if not diagnostics.available:
        lines.append(f"       unavailable: {diagnostics.reason}")
        return lines
    if not diagnostics.cache_records:
        lines.append(f"       (no {model.CACHE_RECORD_PREFIX} records in "
                     "this log)")
        return lines
    for record in diagnostics.cache_records:
        lines.append(f"       {record}")
    return lines


def render_aggregate(block: model.Aggregate) -> list[str]:
    total = block.sample_count + len(block.excluded)
    lines = [f"== {block.label} -- {block.sample_count} successful sample(s) "
             f"of {total} run(s) in this selection"]
    if block.sample_count == 0:
        lines.append("   no successful runs in this selection; no median or "
                     "p95 is computed for this category")
    else:
        seconds = block.run_seconds()
        lines.append(f"   run wall time    "
                     f"median {model.format_duration(model.median(seconds))}"
                     f"   p95 "
                     f"{model.format_duration(model.percentile(seconds, 0.95))}"
                     f"   n={len(seconds)}")
        samples = block.step_samples()
        lines.append("   steps by median (job / step)")
        if not samples:
            lines.append("       (no timed, non-skipped step in the sample)")
        ordered = sorted(samples.items(),
                         key=lambda item: -(model.median(item[1]) or 0.0))
        for (job, step), values in ordered:
            lines.append(
                f"       median "
                f"{model.format_duration(model.median(values)):>16}   p95 "
                f"{model.format_duration(model.percentile(values, 0.95)):>16}"
                f"   n={len(values)}   {job} / {step}")
    if block.excluded:
        counts: dict[str, int] = {}
        for run in block.excluded:
            counts[run.outcome] = counts.get(run.outcome, 0) + 1
        summary = ", ".join(f"{name} {count}"
                            for name, count in sorted(counts.items()))
        lines.append(f"   not sampled: {summary}")
        for run in block.excluded:
            lines.append(f"       {run.run_id}  {run.outcome:<14} "
                         f"{run.branch}  {run.url}")
    return lines


def render_report(entries, threshold: float) -> list[str]:
    """The whole report: one block per run, then the two aggregates.

    `entries` is a sequence of `(run, diagnostics, pull_request)`.
    """
    lines: list[str] = []
    for run, diagnostics, pull_request in entries:
        lines.extend(render_run(run, diagnostics, pull_request, threshold))
        lines.append("")
    aggregates, other = model.aggregate(run for run, _, _ in entries)
    for block in aggregates:
        lines.extend(render_aggregate(block))
        lines.append("")
    if other:
        lines.append(f"== other events -- {len(other)} run(s) in neither "
                     "category, contributing to no percentile")
        for run in other:
            lines.append(f"       {run.run_id}  {run.event:<16} "
                         f"{run.outcome:<14} {run.url}")
        lines.append("")
    return lines


# ── Commands ──────────────────────────────────────────────────────────


def collect(repo: str, runs: list[model.RunTiming]):
    cache: dict[str, int | None] = {}
    for run in runs:
        yield (run, fetch_log(repo, run), resolve_pull_request(repo, run,
                                                               cache))


def run_report(args: argparse.Namespace) -> int:
    repo = args.repo or resolve_repo()
    workflow_id, workflow_name = resolve_ci_workflow(repo)
    if args.run is not None:
        run = fetch_run(repo, args.run)
        mismatch = workflow_mismatch(run, workflow_id, workflow_name)
        if mismatch is not None:
            raise ReportError(mismatch)
        runs = [run]
    else:
        runs = fetch_runs(repo, workflow_id, args.last, args.event,
                          args.branch)
        if not runs:
            print(f"ci_timing_report.py: no {workflow_name} runs matched "
                  "this selection.")
            return 0
    for line in render_report(list(collect(repo, runs)), args.threshold):
        print(line)
    return 0


def main_self_test() -> int:
    """Delegate to the self-test owner's own entry point.

    Delegating rather than reimplementing the reporting is what keeps
    `selftestlib.concluded`'s vacuity guard on this path too: a
    `--self-test` that printed its own "all checks passed" could say so
    over an emptied check registry. It is also why this file does not
    import `selftestlib` itself -- a report command has no verdict to
    route through `concluded`, and `tools/test_selftestlib.py`'s roster
    holds every importer to that contract. Per-assertion narration is
    `python3 tools/test_ci_timing_report.py -v`.
    """
    # Imported HERE rather than at module scope: the self-test owner
    # imports this facade to drive its rendering, and a module-level
    # import either way would be a cycle.
    import test_ci_timing_report

    return test_ci_timing_report.main()


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Report CI lane and step timings for one run or the "
                    "last N, plus the probe and cache diagnostics each run "
                    "recorded.")
    selection = parser.add_mutually_exclusive_group()
    selection.add_argument("--run", type=int, metavar="ID",
                           help="report one run of the CI workflow by id")
    selection.add_argument("--last", type=int, metavar="N",
                           help="report the last N CI runs matching the "
                                "filters below")
    selection.add_argument("--self-test", action="store_true",
                           help="run this report's fixture checks instead "
                                "of contacting GitHub")
    parser.add_argument("--repo", metavar="OWNER/NAME",
                        help="repository to report on (default: the one "
                             "this checkout points at)")
    parser.add_argument("--event", choices=[model.CATEGORY_PULL_REQUEST,
                                            model.CATEGORY_PUSH],
                        help="only runs triggered by this event")
    parser.add_argument("--branch", metavar="NAME",
                        help="only runs whose head branch is NAME")
    parser.add_argument("--threshold", type=float, default=DEFAULT_THRESHOLD,
                        metavar="SECONDS",
                        help="omit steps shorter than this from each run "
                             f"block (default: {DEFAULT_THRESHOLD:.0f})")
    args = parser.parse_args(argv)

    if args.self_test:
        return main_self_test()
    if args.run is None and args.last is None:
        parser.error("pass --run ID, --last N, or --self-test")
    if args.last is not None and args.last < 1:
        parser.error("--last takes a positive count")
    if args.run is not None and (args.event or args.branch):
        parser.error("--event and --branch filter a --last selection; a "
                     "--run names one run outright")
    try:
        return run_report(args)
    except ReportError as error:
        print(f"ci_timing_report.py: {error}")
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
