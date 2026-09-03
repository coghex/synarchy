#!/usr/bin/env python3
"""Fixture-driven checks for the CI timing report (#2277).

Reached through `python3 tools/ci_timing_report.py --self-test`, and
runnable on its own. NOTHING here contacts the network: every run is
read from `tools/fixtures/ci_timing/<case>/`, which holds a recorded
`gh run view --json ...` payload (`meta.json`), its `--json jobs`
companion (`jobs.json`), and an excerpt of the run's `gh run view --log`
output (`run.log`) framed exactly as `gh` frames it -- tabs, the
occasional UTF-8 BOM, and ANSI colour inside the payload included. A
case with no `run.log` is a run whose log GitHub does not publish.

The five cases, and what each is here to pin
--------------------------------------------
  pr_success    a successful pull-request run: a RETRIED probe, a
                skipped step, a step whose timestamps GitHub did not
                publish at all, both cache records, and the probe
                selection line.
  push_success  a master push, which runs NO probe job: the selection
                must read as "not recorded", not as "none selected".
  cancelled     a cancelled run with NO log: metadata and job timings
                still report, probe and cache read unavailable, and the
                run contributes to no percentile.
  pr_timeout    a failed run whose runner died mid-attempt: a `begin`
                with no matching `end` is INCOMPLETE, the attempt that
                did finish is still reported, and a selected probe that
                was never dispatched is named as missing.
  pr_no_probes  a run whose selector chose nothing, and whose cache
                report took the docs-only fast path.

Beside those, three groups of checks that need no fixture: the
estimator (cross-checked against `statistics.quantiles`, not merely
restated), the round trip against
`probe_runner_diagnostics.attempt_identity`, and the STATIC repository
checks -- the three `--print-slow-items=20` command sites, the workflow
comment, the two selection markers this report greps for, and this
command's deliberate absence from both gate files.

Assertions go through `tools/selftestlib.py` (#1922) like every other
`tools/test_*.py`: a satisfied one is silent, a failed one always
prints, `-v` narrates both, and `concluded` refuses a run that executed
no assertion at all -- which is the only tell an emptied `CHECKS`
registry would otherwise leave.
"""
from __future__ import annotations

import dataclasses
import json
import os
import re
import statistics
import sys
from pathlib import Path

# `tools/` has no `__init__.py`; see the note in ci_timing_model.py.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ci_timing_model as model
import ci_timing_report as report
import probe_runner_diagnostics as diagnostics
import selftestlib
from selftestlib import FAILURES, expect

REPO_ROOT = Path(__file__).resolve().parent.parent
FIXTURES = Path(__file__).resolve().parent / "fixtures" / "ci_timing"
WORKFLOW_PATH = REPO_ROOT / ".github" / "workflows" / "ci.yml"
LOCAL_GATE_PATH = REPO_ROOT / "tools" / "ci-local.sh"

#: The exact flag every headless-suite invocation must carry, and the
#: spelling it must carry it in. `--print-slow-items` is Hspec's, not
#: cabal's, so it can only reach the runner through `--test-options`.
SLOW_ITEMS_FLAG = "--test-options='--print-slow-items=20'"
#: The invocation the flag has to sit on, at every site.
HEADLESS_INVOCATION = "cabal test synarchy-test-headless"
#: How many sites there are, and where. Two conditional branches in the
#: workflow (full tier and fast tier) and one in the local gate. The
#: PR's own CI run can execute only ONE of the workflow's two branches,
#: so all three are verified statically here instead.
EXPECTED_WORKFLOW_SITES = 2
EXPECTED_LOCAL_SITES = 1

CASES = ("pr_success", "push_success", "cancelled", "pr_timeout",
         "pr_no_probes")


def load_case(name: str) -> tuple[model.RunTiming, model.LogDiagnostics]:
    """One fixture as the report sees it, with no network access."""
    directory = FIXTURES / name
    meta = json.loads((directory / "meta.json").read_text(encoding="utf-8"))
    jobs = json.loads((directory / "jobs.json").read_text(encoding="utf-8"))
    run = model.build_run(meta, jobs)
    log = directory / "run.log"
    if not log.is_file():
        # Exactly what `fetch_log` does for a run GitHub publishes no
        # downloadable log for.
        return run, model.LogDiagnostics.unavailable(
            "no log archive is published for this run")
    return run, model.read_log(log.read_text(encoding="utf-8"))


def probe(payload: model.LogDiagnostics, key: str) -> model.ProbeSummary | None:
    for summary in payload.probes:
        if summary.key == key:
            return summary
    return None


def step_named(run: model.RunTiming, job: str,
               name: str) -> model.StepTiming | None:
    for candidate in run.jobs:
        if candidate.name != job:
            continue
        for step in candidate.steps:
            if step.name == name:
                return step
    return None


def check_timestamp_endpoints() -> None:
    """Run/job/step wall times come from the documented endpoint pairs."""
    run, _ = load_case("pr_success")
    # startedAt 18:19:21 -> updatedAt 18:40:16 is 1255 s. createdAt is
    # 18:19:11, so a run time of 1265 would mean the queue wait had been
    # folded in, which the module docstring promises it is not.
    expect(run.seconds == 1255.0,
           f"run wall time is startedAt -> updatedAt (1255.0s, queue wait "
           f"excluded), not {run.seconds}")
    expect(run.queued_seconds == 10.0,
           f"queue wait is createdAt -> startedAt (10.0s), not "
           f"{run.queued_seconds}")
    job = next((item for item in run.jobs if item.name == "test-and-audits"),
               None)
    expect(job is not None and job.seconds == 1207.0,
           f"a job's wall time is startedAt -> completedAt (1207.0s), not "
           f"{job and job.seconds}")
    headless = step_named(run, "test-and-audits", "Headless test suite")
    expect(headless is not None and headless.seconds == 315.0,
           f"a step's wall time is startedAt -> completedAt (315.0s), not "
           f"{headless and headless.seconds}")


def check_missing_timestamps() -> None:
    """A step with no timestamps is unavailable, never zero."""
    run, log = load_case("pr_success")
    orphan = step_named(run, "test-and-audits", "probe runner self-tests")
    if not expect(orphan is not None,
                  "the pr_success fixture still carries its "
                  "missing-timestamp step"):
        return
    expect(orphan.seconds is None,
           f"a step with no timestamps has no duration, not {orphan.seconds} "
           "-- 0.0 would sink every median it entered")
    expect(not orphan.is_sample,
           "a step with no timestamps is not an aggregate sample")
    # And it must not be silently invisible: the run block says so.
    lines = report.render_run(run, log, None, 30.0)
    expect(any("probe runner self-tests" in line and "missing timestamp" in line
               for line in lines),
           "the run block names the step whose timestamps are missing")
    # A skipped step has REAL, equal endpoints -- a genuine zero -- and
    # is excluded for a different reason.
    skipped = step_named(run, "test-and-audits", "Graphical test suite build")
    if expect(skipped is not None and skipped.seconds == 0.0
              and skipped.skipped,
              "a skipped step keeps its real, equal-endpoint zero duration"):
        expect(not skipped.is_sample,
               "a skipped step is not an aggregate sample")
    blocks, _ = model.aggregate([run])
    samples = blocks[0].step_samples()
    for key in (("test-and-audits", "probe runner self-tests"),
                ("test-and-audits", "Graphical test suite build")):
        expect(key not in samples, f"{key} stays out of the step aggregate")


def check_slowest_job() -> None:
    """The marked job is the longest non-skipped one, ties included."""
    run, _ = load_case("pr_success")
    marked = model.slowest_jobs(run)
    expect(marked == frozenset({"test-and-audits"}),
           f"the longest job is marked ('test-and-audits'), not "
           f"{sorted(marked)}")
    # A tie must mark BOTH, not pick one arbitrarily, and a skipped job
    # must never win however long its recorded span.
    tied = dataclasses.replace(run, jobs=(
        model.JobTiming("a", "completed", "success", None, None, 90.0, ()),
        model.JobTiming("b", "completed", "success", None, None, 90.0, ()),
        model.JobTiming("c", "completed", "success", None, None, 10.0, ()),
        model.JobTiming("d", "completed", "skipped", None, None, 900.0, ()),
    ))
    expect(model.slowest_jobs(tied) == frozenset({"a", "b"}),
           "a tie for longest marks every tied job, and a skipped job never "
           "takes the mark")
    expect(not model.slowest_jobs(dataclasses.replace(run, jobs=())),
           "a run with no jobs marks no slowest job")


def check_slow_steps() -> None:
    """The per-run step list honours its threshold and its order."""
    run, _ = load_case("pr_success")
    names = [step.name for step in model.slow_steps(run, 30.0)]
    expected = ["Behavior probes (path-selective)", "Headless test suite",
                "Build test suites", "Build (library + executable)"]
    expect(names == expected,
           f"steps at or above 30s are the four long ones, longest first; "
           f"got {names}")
    expect([step.name for step in model.slow_steps(run, 400.0)]
           == ["Behavior probes (path-selective)"],
           "raising the threshold shrinks the step list")
    expect(not model.slow_steps(run, 10_000.0),
           "an unreachable threshold selects no step")


def check_probe_attempts() -> None:
    """A retried probe reports every attempt it actually consumed."""
    _, log = load_case("pr_success")
    expect(log.selection == ("canteen_instance", "craft",
                             "persistence_contract"),
           f"the selection line yields its three probe keys, not "
           f"{log.selection}")
    retried = probe(log, "canteen_instance")
    if not expect(retried is not None, "the retried probe produced a summary"):
        return
    expect(retried.attempt_count == 2,
           f"the retried probe reports the 2 attempts OBSERVED, not the "
           f"identity's denominator; got {retried.attempt_count}")
    expect(retried.budget == 2, "the retry budget is read from the identity")
    expect([attempt.status for attempt in retried.attempts]
           == ["FAIL", "PASS"],
           "each attempt keeps its own status")
    expect([attempt.seconds for attempt in retried.attempts] == [35.6, 50.9],
           "each attempt keeps its own duration")
    expect(retried.consumed_seconds == 86.5,
           f"the probe's consumed duration is the TOTAL across attempts "
           f"(86.5s), not the last one; got {retried.consumed_seconds}")
    expect(retried.final_status == "PASS",
           "the final status is the last attempt's")
    expect(not retried.incomplete,
           "a fully paired probe reports no incomplete attempt")
    single = probe(log, "persistence_contract")
    expect(single is not None and single.consumed_seconds == 206.0
           and single.attempt_count == 1,
           "the single-attempt probe reports one attempt of 206.0s")
    # Pairing is by identity, so a retry must not steal the batch
    # attempt's `end`, and two probes in flight must not cross.
    other = probe(log, "craft")
    expect(other is not None and other.consumed_seconds == 85.0,
           "interleaved probes keep their own begin/end pairing")


def check_incomplete_attempt() -> None:
    """A `begin` with no `end` is incomplete, and says so."""
    run, log = load_case("pr_timeout")
    stalled = probe(log, "expedition_loop")
    if not expect(stalled is not None,
                  "the probe that never finished still produced a summary"):
        return
    expect(len(stalled.incomplete) == 1,
           f"the unmatched begin is reported as exactly one incomplete "
           f"attempt; got {len(stalled.incomplete)}")
    expect(stalled.consumed_seconds is None,
           "an attempt that never ended contributes no duration")
    expect(stalled.final_status is None,
           "an attempt that never ended contributes no status")
    finished = probe(log, "craft")
    expect(finished is not None and finished.consumed_seconds == 85.0,
           "the attempt that DID finish survives beside the incomplete one")
    lines = report.render_probes(log)
    expect(any("INCOMPLETE" in line for line in lines),
           "the run block marks the incomplete attempt")
    expect(any("selected but no records" in line for line in lines),
           "a selected probe that was never dispatched is named as missing")
    expect(not run.successful, "a failed run is not an aggregate sample")


def check_no_probe_job() -> None:
    """A master push has no probe job, which is not an empty selection."""
    run, log = load_case("push_success")
    expect(not any(job.name == "behavior-probes" for job in run.jobs),
           "the master-push fixture still has no behavior-probes job")
    expect(log.selection is None and not log.selection_empty,
           "a run with no probe job reports no selection either way")
    expect(not log.probes, "a run with no probe job reports no attempts")
    expect(any("no probe job ran" in line
               for line in report.render_probes(log)),
           "'no probe job' renders differently from 'no probes selected'")
    # The other way round: a run whose SELECTOR chose nothing.
    _, empty = load_case("pr_no_probes")
    expect(empty.selection_empty, "the empty-selection line is recognised")
    expect(any("the selector chose no probe" in line
               for line in report.render_probes(empty)),
           "an empty selection renders as one")


def check_cache_records() -> None:
    """Every CI_CACHE_REPORT record is collected, not just one line."""
    _, log = load_case("pr_success")
    expect(len(log.cache_records) == 2,
           f"a normal run yields one record per cache (2), not "
           f"{len(log.cache_records)}")
    expect(all(record.startswith(model.CACHE_RECORD_PREFIX)
               for record in log.cache_records),
           "every collected record carries the cache-report prefix")
    _, push = load_case("push_success")
    expect(len(push.cache_records) == 2
           and "FULL_MISS" in push.cache_records[1],
           "the master push keeps both of its cache records")
    _, docs = load_case("pr_no_probes")
    expect(docs.cache_records
           == ("CI_CACHE_REPORT skipped=docs-only-fast-path",),
           f"the docs-only fast path's single record is collected; got "
           f"{docs.cache_records}")


def check_log_unavailable() -> None:
    """A cancelled run reports metadata and marks diagnostics unavailable."""
    run, log = load_case("cancelled")
    expect(not log.available,
           "a run with no log archive reports its log unavailable")
    expect(not run.successful, "a cancelled run is not an aggregate sample")
    expect(run.outcome == "cancelled",
           f"a cancelled run is counted under 'cancelled', not {run.outcome!r}")
    # A job that never completed has no duration -- and must not abort
    # the block.
    stalled = next((job for job in run.jobs if job.name == "behavior-probes"),
                   None)
    expect(stalled is not None and stalled.seconds is None,
           "an unfinished job reports no duration")
    text = "\n".join(report.render_run(run, log, None, 30.0))
    expect(run.url in text, "a run with no log keeps its own metadata")
    expect(text.count("unavailable: ") >= 2,
           "the probe and cache sections are both marked unavailable")


def check_estimator() -> None:
    """The median/p95 estimator is R-7, and says so by agreeing with it."""
    expect(model.percentile([], 0.5) is None,
           "an empty sample has no percentile")
    expect(model.median([]) is None, "an empty sample has no median")
    expect(model.percentile([7.0], 0.95) == 7.0,
           "a one-element sample is itself")
    expect(model.median([1.0, 2.0, 3.0]) == 2.0,
           "an odd-length median is the middle value")
    expect(model.median([1.0, 2.0, 3.0, 4.0]) == 2.5,
           "an even-length median is the mean of the two middle values")
    expect(model.median([3.0, 1.0, 2.0]) == 2.0,
           "the estimator does not depend on input order")
    expect(model.percentile([1.0, None, 3.0], 0.5) == 2.0,
           "an unavailable value is excluded from the sample")
    # Not a restatement: the same figure computed by the standard
    # library's inclusive method, which is what R-7 means.
    for sample in ([1.0, 2.0, 3.0, 4.0, 5.0],
                   [10.0, 12.0, 11.0, 40.0, 13.0, 12.5, 90.0],
                   [1207.0, 1180.0, 1301.0, 1150.0]):
        reference = statistics.quantiles(sample, n=100, method="inclusive")[94]
        expect(abs(model.percentile(sample, 0.95) - reference) < 1e-9,
               f"p95 of {sample} matches the inclusive (R-7) estimator "
               f"({reference})")
        expect(abs(model.median(sample) - statistics.median(sample)) < 1e-9,
               f"median of {sample} matches statistics.median")


def check_aggregate_categories() -> None:
    """Only successful runs are sampled; the rest are counted separately."""
    runs = [load_case(name)[0] for name in CASES]
    blocks, other = model.aggregate(runs)
    by_name = {block.category: block for block in blocks}
    pull = by_name[model.CATEGORY_PULL_REQUEST]
    push = by_name[model.CATEGORY_PUSH]
    expect(pull.sample_count == 2,
           f"the two successful pull-request runs are the only samples; got "
           f"{pull.sample_count}")
    expect({run.outcome for run in pull.excluded} == {"cancelled", "failure"},
           "the cancelled and the failed run are both excluded, and counted")
    expect(push.sample_count == 1 and not push.excluded,
           "the master-push category is one clean sample")
    expect(not other, f"no run falls outside both categories; got {len(other)}")
    # Steps aggregate by (job, step): the same step name in two jobs
    # must stay two entries.
    samples = pull.step_samples()
    setups = [key for key in samples if key[1] == "Set up job"]
    expect(len(setups) == 2,
           f"'Set up job' occurs in two jobs and stays two aggregate entries; "
           f"got {len(setups)}")
    headless = samples.get(("test-and-audits", "Headless test suite"))
    expect(headless is not None and len(headless) == 2,
           "the headless step accumulates one sample per successful "
           "pull-request run")
    # An empty category renders explicitly rather than as a zero.
    rendered = "\n".join(report.render_aggregate(
        model.Aggregate(model.CATEGORY_PUSH, "master pushes")))
    expect("no median or p95" in rendered,
           "an empty category renders explicitly")
    expect("0s" not in rendered, "an empty category renders no zero duration")


def check_log_framing() -> None:
    """`gh`'s framing is undone exactly, and nothing else is."""
    text = (
        "job-a\tstep-one\t﻿2026-09-02T18:19:26.1900089Z "
        "\x1b[36;1mCI_CACHE_REPORT cache=x outcome=EXACT_HIT\x1b[0m\n"
        "job-b\tstep-two\t2026-09-02T18:19:27.0000000Z plain payload\n"
        "not a framed line at all\n"
        "job-c\tstep-three\tno timestamp here\n"
    )
    records = list(model.log_records(text))
    if not expect(len(records) == 2,
                  f"an unframed line and one with no timestamp are dropped, "
                  f"leaving 2 records; got {len(records)}"):
        return
    expect(records[0].payload == "CI_CACHE_REPORT cache=x outcome=EXACT_HIT",
           f"the BOM and the ANSI colour are stripped; got "
           f"{records[0].payload!r}")
    expect((records[0].job, records[0].step) == ("job-a", "step-one"),
           "the job and step attribution survives framing")
    expect(records[1].payload == "plain payload",
           "an unadorned payload is unaltered")
    # A payload containing a tab must survive: only the first two tabs
    # are structural.
    tabbed = list(model.log_records(
        "j\ts\t2026-09-02T18:19:27.0000000Z left\tright\n"))
    expect(bool(tabbed) and tabbed[0].payload == "left\tright",
           "a payload containing a tab is not truncated")


def check_identity_round_trip() -> None:
    """The identity reader agrees with the canonical formatter."""
    for key, script, attempt, total in (
            ("craft", "craft_probe.py", 1, 2),
            ("persistence_contract", "persistence_contract_probe.py", 2, 3),
            ("blood_gpu_lifecycle", "blood_gpu_lifecycle_probe.py", 10, 10)):
        identity = diagnostics.attempt_identity(key, script, attempt, total)
        parsed = model.parse_attempt_identity(identity)
        expect(parsed == (key, script, attempt, total),
               f"{identity!r} round-trips through the canonical formatter; "
               f"got {parsed}")
    for rejected in ("", "craft", "craft (craft_probe.py)",
                     "craft (craft_probe.py) attempt one/two"):
        expect(model.parse_attempt_identity(rejected) is None,
               f"{rejected!r} is not accepted as an attempt identity")
    for detail, wanted in (("PASS (35.6s)", ("PASS", 35.6)),
                           ("TIMEOUT (900.0s)", ("TIMEOUT", 900.0)),
                           ("dispatched", (None, None))):
        expect(model.parse_attempt_outcome(detail) == wanted,
               f"the outcome of {detail!r} is {wanted}, not "
               f"{model.parse_attempt_outcome(detail)}")


def _headless_invocations(text: str) -> list[str]:
    """Every executable `cabal test synarchy-test-headless` line.

    Comment lines are dropped first: a commented-out invocation does not
    run, so counting one would let a real site go missing behind it.
    """
    return [line.strip() for line in text.splitlines()
            if not line.strip().startswith("#")
            and HEADLESS_INVOCATION in line]


def check_slow_items_wiring() -> None:
    """All three command sites carry the flag, plus the step comment.

    Static on purpose. The PR's own CI run executes exactly ONE of the
    workflow's two conditional branches -- whichever its worldgen
    selector picks -- so the other branch and the local gate can only be
    verified by reading the files. `tools/ci_parity_audit.py` cannot
    help either: it compares `python3 tools/*.py` commands and
    deliberately ignores every `cabal test` line.
    """
    workflow = WORKFLOW_PATH.read_text(encoding="utf-8")
    local = LOCAL_GATE_PATH.read_text(encoding="utf-8")

    sites = _headless_invocations(workflow)
    expect(len(sites) == EXPECTED_WORKFLOW_SITES,
           f"{WORKFLOW_PATH.name} runs the headless suite "
           f"{EXPECTED_WORKFLOW_SITES} time(s); got {len(sites)}, and a new "
           "site needs the slow-items flag too")
    for site in sites:
        expect(SLOW_ITEMS_FLAG in site,
               f"{WORKFLOW_PATH.name}'s headless invocation carries "
               f"{SLOW_ITEMS_FLAG}: {site}")

    local_sites = _headless_invocations(local)
    expect(len(local_sites) == EXPECTED_LOCAL_SITES,
           f"{LOCAL_GATE_PATH.name} runs the headless suite "
           f"{EXPECTED_LOCAL_SITES} time(s); got {len(local_sites)}")
    for site in local_sites:
        expect(SLOW_ITEMS_FLAG in site,
               f"{LOCAL_GATE_PATH.name}'s headless invocation carries "
               f"{SLOW_ITEMS_FLAG}: {site}")

    # The step's own comment has to tell a reader why the list is there.
    step = re.search(r"\n((?:      #[^\n]*\n)+)      - name: Headless test "
                     r"suite\n", workflow)
    if expect(step is not None,
              "the `Headless test suite` step still carries a comment block"):
        expect("slow" in step.group(1).lower(),
               "the `Headless test suite` step comment mentions the "
               "slowest-examples list")


def check_selection_markers() -> None:
    """The workflow still prints the two lines this report greps for."""
    workflow = WORKFLOW_PATH.read_text(encoding="utf-8")
    expect(f'echo "{model.SELECTION_PREFIX}$ONLY"' in workflow,
           f"{WORKFLOW_PATH.name} still echoes {model.SELECTION_PREFIX!r}, "
           "without which every probe block reads 'selection not recorded'")
    expect(model.SELECTION_EMPTY in workflow,
           f"{WORKFLOW_PATH.name} still prints {model.SELECTION_EMPTY!r}, "
           "without which an empty selection is indistinguishable from an "
           "absent probe job")


def check_not_a_gate() -> None:
    """This command runs in neither CI nor `make ci`, by design.

    CIR-1 forbids turning a timing band into a failing check without a
    maintainer decision, and requirement 6 of #2277 asks for a green
    `ci_parity_audit.py` with no exemption edit. Adding the command to
    exactly one of the two files is what would break that audit; adding
    it to both would make every run pay for a diagnostic.
    """
    for path in (WORKFLOW_PATH, LOCAL_GATE_PATH):
        invoked = [line for line in path.read_text(encoding="utf-8").splitlines()
                   if line.strip().lstrip("#").strip().startswith(
                       "python3 tools/ci_timing_report.py")]
        expect(not invoked,
               f"{path.name} does not invoke ci_timing_report.py; it is an "
               f"on-demand diagnostic, not a gate: {invoked}")


def check_report_renders() -> None:
    """The whole report renders over every fixture without raising."""
    entries = []
    for name in CASES:
        run, log = load_case(name)
        entries.append((run, log, 2340 if name == "pr_success" else None))
    text = "\n".join(report.render_report(entries, 30.0))
    for wanted in ("<- slowest job", "PR #2340 (head issue-2091",
                   "branch master", "pull-request runs", "master pushes",
                   "not sampled:", "CI_CACHE_REPORT",
                   "canteen_instance", "INCOMPLETE"):
        expect(wanted in text, f"the rendered report mentions {wanted!r}")
    for run, _, _ in entries:
        expect(str(run.run_id) in text, f"run {run.run_id} is in the report")


def check_run_selection_guard() -> None:
    """A `--run` naming another workflow is refused, not reported."""
    run, _ = load_case("pr_success")
    ci_id = run.workflow_id
    if not expect(ci_id is not None,
                  "the pr_success fixture still carries a workflowDatabaseId"):
        return
    expect(report.workflow_mismatch(run, ci_id, "CI") is None,
           "a run of the CI workflow is accepted")
    foreign = dataclasses.replace(run, workflow_id=305662624,
                                  workflow_name="review-gate")
    refusal = report.workflow_mismatch(foreign, ci_id, "CI")
    if expect(refusal is not None,
              "a run of another workflow is refused, not reported as a CI "
              "run"):
        expect("review-gate" in refusal
               and report.CI_WORKFLOW_PATH in refusal,
               f"the refusal names both workflows: {refusal}")
    # A payload with no workflow id contradicts nothing and is reported.
    expect(report.workflow_mismatch(dataclasses.replace(run,
                                                        workflow_id=None),
                                    ci_id, "CI") is None,
           "a payload carrying no workflow id is not refused")
    expect(report.CI_WORKFLOW_PATH == ".github/workflows/ci.yml",
           "the report selects the CI workflow by its path, so an event "
           "filter alone cannot mix in review-gate and ntfy-notify runs")
    expect((REPO_ROOT / report.CI_WORKFLOW_PATH).is_file(),
           f"{report.CI_WORKFLOW_PATH} exists")


#: Every check group, in the order `--self-test` runs them.
CHECKS = (
    ("timestamp endpoints", check_timestamp_endpoints),
    ("missing timestamps", check_missing_timestamps),
    ("slowest job", check_slowest_job),
    ("per-run slow steps", check_slow_steps),
    ("probe attempts and retries", check_probe_attempts),
    ("incomplete probe attempt", check_incomplete_attempt),
    ("no probe job", check_no_probe_job),
    ("cache records", check_cache_records),
    ("unavailable log", check_log_unavailable),
    ("estimator", check_estimator),
    ("aggregate categories", check_aggregate_categories),
    ("log framing", check_log_framing),
    ("attempt identity round trip", check_identity_round_trip),
    ("slow-items wiring", check_slow_items_wiring),
    ("selection markers", check_selection_markers),
    ("not a gate", check_not_a_gate),
    ("report rendering", check_report_renders),
    ("run selection guard", check_run_selection_guard),
)


def main() -> int:
    selftestlib.parse_verbose()
    for _label, check in CHECKS:
        check()
    if FAILURES:
        print(f"{len(FAILURES)} self-test failure(s) above.")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, f"ci_timing_report self-test: all {len(CHECKS)} check group(s) pass")


if __name__ == "__main__":
    raise SystemExit(main())
