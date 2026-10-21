# Headless CI hang diagnostics and investigation (#2747, CIR-3)

Evidence for PR #2819. Everything here is an observed fact unless it is
labelled *inference*. CI logs and artifacts expire, so the decisive excerpts
are kept in this file; run and job ids are given so the originals can be
opened while they last.

## 1. What was delivered

`Test.Headless.Progress` wraps the headless spec tree's own examples and
fixture cleanups. For a run whose environment sets
`SYNARCHY_HEADLESS_PROGRESS_FILE` (CI sets it on the `cabal test` command of
each `headless-lanes` leg) it keeps a small text record current: the active
example, the preceding completed example, and any shared-fixture teardown in
flight. A step after the lane, `Headless active context` (`if: always()`),
prints that file when it still says `state: running`. The procedure is in
`tools/README.md`, "Investigating a hung headless run (#2747)".

## 2. Both stop paths reach the CI record

All headless-lane hangs below are deliberate and live on temporary draft PRs
(#2820 to #2823), each PR #2819's implementation plus one hang commit. No
timer or timeout was added; the job's own `timeout-minutes: 90` is unchanged.

### 2.1 Manual cancellation (final mechanism)

Run 38304749179, PR #2821, head `fa5a6c289` = PR #2819 `04ce0f3f6` plus an
example that sleeps forever. `gh run cancel` requested at 2026-10-12T20:39:40Z;
the runner reported `context canceled` for `Headless lane` and the
`Headless active context` step ran and printed (job 114996167797, lane `rest`):

```
headless-active-context v1
lane: rest
state: running
completed examples: 1
active example: Diag.Hang.Example / hangs here (temporary #2747 demonstration)
preceding completed example: Diag.Hang.Example / completes before the hang
fixture boundary: none
```

The `world` lane of the same run (job 114996167789) was not hung, only mid-run,
and printed the example it had reached when the cancel landed:

```
lane: world
state: running
completed examples: 215
active example: map pyramid finest-page goldens (#2298) / matches the pinned chunk bytes at worldSize 128
preceding completed example: map pyramid finest-page goldens (#2298) / matches the pinned chunk bytes at worldSize 64
fixture boundary: none
```

### 2.2 Expiry of the existing 90 minute job timeout (final mechanism)

Run 38304752870, PR #2823, head `7bf88c62d` = PR #2819 `04ce0f3f6` plus a
shared fixture (`aroundAll`) whose release never returns, after two examples
that pass. Job 114996324930 (`headless-lanes (rest)`) started 20:32:18Z and was
cancelled by the runner at 22:02:30Z (90 minutes), with no human action. Its
`Headless active context` step then printed:

```
headless-active-context v1
lane: rest
state: running
completed examples: 2
active example: none
preceding completed example: Diag.Hang.Fixture / also completes before the teardown hang
fixture boundary: aroundAll cleanup closing "Diag.Hang.Fixture" (inside the suite root); no example is active
```

This is the shared-fixture boundary case: no example is active, the group and
boundary are named, and the preceding completed example is the last one that
actually finished (not the next example, which Hspec has already announced by
the time a teardown runs).

The `world` lane of that run completed normally (job 114996324762); its
`Headless active context` step printed nothing, so a finished lane adds no
log content.

### 2.3 Earlier attempts, kept because they changed the design

* **Run 38298805604 (PR #2821, first head), cancelled 19:46:05Z.** The
  `Headless active context` step ran but printed nothing, and the test
  process printed nothing on stderr either. Two findings: `${{ runner.temp }}`
  is the HOST path (`/home/runner/work/_temp`) in a container job, which the
  container cannot write, so the record was never created; and the process
  printed no "interrupted by signal" text, i.e. the runner stops the container
  process without signalling it. The recorder now warns once on stderr when it
  cannot write its file, the path comes from the shell's `$RUNNER_TEMP`, and
  the file (read by the `always()` step), not a signal handler, is the
  mechanism. The handler remains for a local Ctrl-C.
* **Run 38300205747 (PR #2821, second head), cancelled 20:29:21Z.** With the
  path fixed (variable exported through `$GITHUB_ENV`), cancellation delivered
  the record (job 114992450730). That mechanism added one line to a passing
  log (the step's `env:` listing), so it was replaced by setting the variable
  on the command itself, which produced the final-mechanism runs in 2.1 and
  2.2.
* **Run 38300201226 (PR #2820), job timeout at 21:20:31Z** (`$GITHUB_ENV`
  mechanism): the 90 minute expiry also ran the `always()` step and printed the
  same fixture-boundary record (job 114981256167). This confirmed the
  timeout path one hour before the final-mechanism run completed.

## 3. Passing runs, failing examples, and presentation

* **Passing output is not larger than master's.** `Headless lane` step log
  lines, master run 38296352661 (`6651df5c6`, PR #2819's base) against PR run
  38303783525 (`04ce0f3f6`): `world` 38 and 38, `rest` 85 and 85. Both end
  with the slow-items list and the footer
  (`Finished in 669.7215 seconds`, `10027 examples, 0 failures` for `rest` on
  the PR head), so summaries and slow-item reporting are retained. A step
  `env:` would have added a line, which is why the variable is set on the
  command.
* **A failing example keeps its full details and the failure exit.** The
  suite executable run locally with CI's flags and the recorder on, with a
  deliberately failing example (not committed):

  ```
  Failures:

    test-headless/Test/Headless/Core/DiagFail.hs:11:38:
    1) Diag.Fail fails on purpose
         expected: 2
          but got: 1

    To rerun use: --match "/Diag.Fail/fails on purpose/" --seed 906778249

  Randomized with seed 906778249

  Finished in 0.0016 seconds
  2 examples, 1 failure
  exit=1
  ```

  and the record ended `state: finished`.
* The final PR head's own CI run is linked from the PR description.

## 4. Historical investigation: master run 36054401429

Observed facts about the original run: push to master at `1ccbedd9c`
(2026-09-24); `test-and-audits` started 20:22:49Z and was cancelled at 21:53:00Z
(the 90 minute timeout); its log named no active example (the last output was
`READY port=46459`, then nothing for 65 minutes). That revision has no
recorder: `test-headless/Spec.hs` calls plain `hspec`.

### 4.1 Unchanged rerun

`gh run rerun 36054401429` on the unchanged commit and unchanged workflow
(attempt 2, started 2026-10-12T19:20:03Z). Observed: the suite completed.
`Headless test suite` ran 19:36:50Z to 19:53:59Z, `Finished in 998.3391
seconds`, `10440 examples, 0 failures`; `test-and-audits` succeeded at 19:57:15Z.
The hang did **not** recur in one rerun. Location: unknown (not reproduced).

### 4.2 Diagnostic replay (not an unchanged rerun)

* **Base:** `1ccbedd9c`, pushed unmodified as branch `diag-2747-base-1ccbedd9c`.
* **Instrumentation:** the recorder module, `main` wrapped in
  `withProgressFromEnv`, `hspec-core` added to the test-suite dependencies, the
  environment variable on the `Headless test suite` step, and a step that
  prints the record. The full diff against the base is
  [`issue-2747-replay-instrumentation.diff.txt`](../pr-proofs/issue-2747-replay-instrumentation.diff.txt).
* **Method:** draft PR #2822 from `diag-2747-replay-1ccbedd9c` onto the base
  branch. A `pull_request` workflow tests the merge of the head into the base
  branch, which here is the historical tree plus the instrumentation; no push
  to master was involved.
* **Environment:** GitHub-hosted `ubuntu-latest` with the CI container image
  resolved for that tree by the workflow itself.
* **Outcome:** run 38300220904, head `59a1a0f0c`. The suite completed: `Headless
  test suite` 20:07:26Z to 20:24:42Z, `Finished in 1003.8627 seconds`, `10440
  examples, 0 failures, 1 pending`; the record step printed nothing
  (`state: finished`). The hang did not recur under instrumentation either.
  Location: unknown (not reproduced).

*Inference, not observation:* two completions of the same tree, with and without
the recorder, say nothing about what stalled on the nine runs of August to
September 2026; the cluster stopped after 2026-09-02 and the tenth hang
(2026-09-24) has now neither recurred nor been localized. The issue's owner
decision accepts "location unknown" / "not reproduced" for this
investigation.

## 5. Local reproduction attempts: `UI.ResponsiveMenus`

Command, in a clean detached worktree of each revision, production profile,
one attempt per invocation, 900 s limit per attempt (a longer attempt would
have been recorded as a hang and its process group terminated):

```
cabal test synarchy-test-headless -v0 --test-show-details=direct \
  --test-options='--match "UI.ResponsiveMenus"'
```

Environment (identical for both): macOS 26.7.1, arm64 (Apple silicon), GHC
9.12.2, cabal-install 3.16.1.0. This is **not** the platform of the recorded
hangs, which ran in the Linux CI container (`ubuntu-latest`); a local "not
reproduced" says nothing about whether the Linux hang is fixed. The attempts
shared the machine with other builds and test runs of this issue (load average
roughly 3 to 12 on 16 cores).

| Revision | Attempts | Outcomes | Wall seconds per attempt | Hspec `Finished in` |
|---|---|---|---|---|
| `6651df5c6` (current master when pinned; PR #2819's base) | 20 | 20 PASS, 0 FAIL, 0 hang | 72 24 22 18 19 33 35 37 37 21 20 19 19 19 24 21 24 20 18 18 | 4.9 to 19.9 s, `140 examples, 0 failures` each |
| `698bc869e` (2026-09-02, the last master commit of the recorded cluster) | 20 | 20 PASS, 0 FAIL, 0 hang | 24 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 | 3.6 to 3.8 s, `110 examples, 0 failures` each |

Builds: 345.6 s and 248.0 s. No attempt hung or failed, so there was nothing to
narrow with `--match` and no diagnostic instrumentation was applied locally.
Narrowed location: none (not reproduced). Fixing a discovered hang would have
belonged to a separate issue; none was discovered.

The per-attempt logs and the driver script are kept under
`~/.local/state/syn-2747/local/` on the machine that ran them.
