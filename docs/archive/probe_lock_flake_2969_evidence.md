# #2969: `test_probe_resource_lock` flake: cause and stability evidence

## Cause

`headless work still starts while a GPU probe waits` failed in CI with
`busy=the shared interest in resource 'cabal-build' is not available, error=None`.
It was not cross-run interference: the test already runs under a private
`Scratch` lock root, passed to both the in-process `taking()` and the waiter
subprocess. It was a race inside the test.

`probe_resource_lock.acquire()` is nonblocking by contract. It also tries the
namespace admission mutex without waiting, and if another admission pass owns
it, raises `ResourceBusy` for the first requested interest (here `cabal-build`)
with no named holders, even though nothing conflicts. The waiter subprocess
polls that mutex every 20 ms, so one `taking()` call that lands in the window
is refused. The scheduler is correct; the test made a single attempt.

Forced deterministically: holding the mutex and calling `taking(shared=HEADLESS_SHARED)`
returns `ResourceBusy(cabal-build)` with `holders == []`; releasing the mutex
makes the same call succeed.

## Fix (test only; `tools/probe_resource_lock.py` unchanged)

- `taking_admitted()` retries a refusal for up to 5 s; a persistent refusal is
  returned as the failure it is, and a `ResourceLockError` is returned at once.
- `taking_refused()` serves cases that expect a refusal naming the queued
  writer; a holder-less refusal is the transient one and is retried.
- Applied to the live-waiter assertions: the failing case, the queued-writer
  refusal and the unrelated-resource case.
- The failing case now also asserts admission happens while the first GPU
  probe still holds `gpu-desktop` and the second is still queued.
- New regression test `test_a_transient_admission_mutex_refusal_is_ridden_out_not_masked`
  forces the interleaving (the mutex is released only after the helper begins
  its second attempt, not on a timer, so a scheduling pause cannot skip the
  refusal; review round 1 found the timer version flaky), and checks that a persistent mutex hold, a real
  resource conflict and a `ResourceLockError` are not masked. Replacing the
  retry with a single attempt makes it fail.

## Evidence

Host: macOS (Darwin 25.6.0, arm64), Python 3.x system interpreter.

Failing case alone, 6 concurrent processes x 40 runs (CPU contention):

| Revision | Failures |
|---|---|
| master `2f6cb83a9` (before) | 8 / 240, all with the CI message above |
| fix `84eb3376b` (after) | 0 / 240 |

Full self-test `python3 tools/test_probe_resource_lock.py` (416 assertions per
run): 50 consecutive runs in each of two separate checkouts (worktrees
`issue-2969-probe-lock-flake` and `issue-2969-second-checkout`), the two loops
overlapping. At `84eb3376b`: 50/50 and 50/50 (also overlapping the load above).
At the final, approved head `9f47924cc`: 50/50 and 50/50, 0 failures.

Acceptance: `python3 tools/test_run_probes.py` (972 assertions) and
`python3 .quruntul/checks.py` (25 tests) pass. No engine run.

Raw progress logs: `~/.local/state/synarchy/issue-2969-evidence/{A,B}.progress` and `round2/{A,B}.progress`.
