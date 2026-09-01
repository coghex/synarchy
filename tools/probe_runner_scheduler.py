#!/usr/bin/env python3
"""Ordering a whole SWEEP of probes, sequentially or `--jobs` at a time.

The aggregate half of the runner (#2074): the sequential path with its
inline retries, the parallel path's resource-aware dispatch (#1322,
#1444, #1436) over the ports the registry allocated (#1571), the solo
retries a parallel failure gets, the cancellation that takes every engine
down on Ctrl-C, the failure presentation both paths share, and the
ordered summary the exit status comes from.

Dependencies (#2074 requirement 11): the registry, resource, diagnostics
and lifecycle owners. Nothing here parses arguments or exits a process —
`tools/run_probes.py` is the command, and it validates its invocation and
constructs these dependencies before handing them here.

`run_with_retry` lives HERE and only here: an attempt is a lifecycle
`lifecycle.run_one` INSIDE a resource hold, so the retry loop is the first
thing
that needs both owners at once, which is exactly what this owner is
permitted to consume.
"""
from __future__ import annotations
import concurrent.futures
import sys
import time

import probe_resource_lock
import probe_runner_diagnostics as diagnostics
import probe_runner_lifecycle as lifecycle
import probe_runner_registry as registry
import probe_runner_resources as resources


def run_with_retry(script, port, timeout, retries, announce=None, groups=None,
                   key=None, namespace=None, waiting=None):
    """Run a probe, re-running SOLO up to `retries` times on failure.

    Returns (status, elapsed, out, attempts). `announce(kind, attempt,
    retries)` is an optional callback for live progress before each retry.
    A retry never starts before the previous attempt's group is reaped —
    `lifecycle.run_one` does that before it returns — so it can never be
    handed a
    port a leaked engine still holds.

    `key` and `namespace` bring EVERY attempt, the first and each retry
    alike, inside the cross-process resource hold (#1436): a retry is
    another full probe execution, so a foreign exclusive holder must be
    able to stop it exactly as it stops the first attempt. `waiting` is an
    optional callback receiving the `ResourceBusy` that is holding this
    attempt up. Passing no `key` leaves the behaviour as it was, which is
    what `tools/test_run_probes.py`'s direct calls rely on.
    """
    attempt = 0
    hold_env = resources.descendant_hold_env(key, namespace) if key else {}
    while True:
        with resources.resource_hold(key, namespace if key else None,
                                     announce=waiting):
            ok, timed_out, elapsed, out = lifecycle.run_one(
                script, port, timeout, groups, hold_env=hold_env)
        attempt += 1
        if ok or attempt > retries:
            break
        if announce:
            announce("TIMEOUT" if timed_out else "FAIL", attempt, retries)
    status = "TIMEOUT" if timed_out else ("PASS" if ok else "FAIL")
    return status, elapsed, out, attempt


def present_failure(out: str, tail: int, indent: str = "    ") -> None:
    """The ONE failure presentation both scheduling paths print (#1768, #1982).

    The two attributions first, then the ordinary tail as context. Both
    are drawn from the COMPLETE capture `lifecycle.run_one` holds, so a
    phase record
    or a failed check that more than `--tail` lines followed is still
    named; nothing else of the capture is printed. The failure records are
    then withheld from the tail, so every recorded failure appears exactly
    once.

    `--jobs` is the OTHER default failure presentation, and both
    guarantees have to hold on it too or they silently lapse whenever a
    probe is selected inside a parallel run — which is why this is one
    function rather than the same block written twice.
    """
    for line in diagnostics.failure_attribution(out):
        print(f"{indent}{line}")
    for line in diagnostics.progress_attribution(out):
        print(f"{indent}{line}")
    for line in diagnostics.without_failure_records(out).splitlines()[-tail:]:
        print(f"{indent}{line}")


def run_sequential(chosen, results, *, port, explicit_timeout, retries, tail,
                   namespace, groups, waiting) -> None:
    """The mode CI relies on: live, ordered, inline retry."""
    n = len(chosen)
    print(f"Running {n} probe(s) sequentially "
          f"({registry.timeout_plan(chosen, explicit_timeout)})...\n")

    def announce(kind, attempt, retries):
        print(f"{kind}, retrying solo ({attempt}/{retries}) ... ",
              end="", flush=True)

    for i, (key, script, _purpose) in enumerate(chosen, 1):
        timeout = registry.effective_timeout(key, explicit_timeout)
        print(f"[{i}/{n}] {script} ... "
              f"[timeout {registry.format_timeout(timeout)}] ",
              end="", flush=True)
        status, elapsed, out, attempts = run_with_retry(
            script, port, timeout, retries, announce,
            groups, key=key, namespace=namespace, waiting=waiting)
        note = (f"  [passed on retry {attempts}]"
                if status == "PASS" and attempts > 1 else "")
        print(f"{status} ({elapsed:.1f}s){note}")
        if status != "PASS" and tail > 0:
            present_failure(out, tail)
        results[key] = (script, status, elapsed, out)


def run_parallel(chosen, results, *, jobs, parallel_base, parallel_ports,
                 explicit_timeout, retries, tail, namespace, groups, progress,
                 total_attempts, waiting) -> None:
    """Parallel (#531) — one independent engine per probe, up to `jobs` at
    once, each on a unique port. That isolates exactly two dimensions and
    claims nothing further: separate processes cannot corrupt each other's
    memory, and unique ports cannot collide on a bind. Every probe still
    drives the SAME checkout, the same build directory and the same
    repository-relative files, so the reader/writer ledger decides what may
    overlap (#1322, #1444) — the scheduling below holds a conflicting probe
    back rather than letting a worker discover the conflict, and takes the
    cross-process interest at the same point (#1436) so a foreign runner or
    measurement cannot overlap it either. Since #1570 the build directory is
    one of the scheduled resources rather than an unguarded one: every probe
    here execs the executable the preflight already resolved, and the three
    GHCi consumers that still run Cabal themselves hold `cabal-build`
    EXCLUSIVELY. Anything not named by either resource table is still
    unguarded. Retries run SOLO afterward, since parallel contention is
    exactly what a retry needs to escape.
    """
    n = len(chosen)
    print(f"Running {n} probe(s), up to {jobs} concurrently "
          f"({registry.timeout_plan(chosen, explicit_timeout)})...\n")

    def work(idx, probe):
        key, script, _ = probe
        timeout = registry.effective_timeout(key, explicit_timeout)
        # `parallel_ports[idx]` is the BASE of this probe's own reserved
        # span, which the allocation laid clear of every other selected
        # probe's (#1571). Never `base + idx`: a two-port probe binds its
        # neighbour's base under that.
        ok, timed_out, elapsed, out = lifecycle.run_one(
            script, parallel_ports[idx], timeout, groups,
            hold_env=resources.descendant_hold_env(key, namespace))
        status = "TIMEOUT" if timed_out else ("PASS" if ok else "FAIL")
        return key, script, status, elapsed, out, timeout

    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as ex:
        # SUBMISSION is inside the try as well as completion: a Ctrl-C
        # partway through it would otherwise leave the `with` directly, and
        # shutdown(wait=True) then runs every future submitted so far —
        # launching probes, and engines, after the interrupt. `futs` is
        # grown as we go so the handler sees exactly what has been
        # submitted.
        futs: list[concurrent.futures.Future] = []
        # Submission is INTERLEAVED with completion rather than done up
        # front, because a probe waiting on a resource someone else holds
        # must not occupy a worker slot that an unrelated ready probe could
        # use. Holding it back here — rather than taking a lock inside the
        # worker — is what keeps `--jobs` worth of real work in flight while
        # a conflict is pending.
        pending = list(enumerate(chosen))
        running: dict[concurrent.futures.Future,
                      tuple[set[str], set[str],
                            probe_resource_lock.ResourceHold]] = {}
        ledger = resources.ResourceLedger()
        done = 0
        foreign = None
        try:
            while pending or running:
                # Dispatch in registry order every probe that fits and whose
                # resources are free; a BLOCKED probe is skipped, never
                # waited on, so later disjoint probes still start.
                foreign = None
                for item in list(pending):
                    if len(running) >= jobs:
                        break
                    i, probe = item
                    need_exclusive = resources.exclusive_resources(probe[0])
                    need_shared = resources.shared_resources(probe[0])
                    if ledger.blocked(need_exclusive, need_shared):
                        continue
                    # The flock request drops whatever an ancestor already
                    # holds exclusively for us (#1570); the ledger above
                    # keeps the full declarations, so this runner still
                    # serialises its own probes.
                    lock_exclusive, lock_shared = resources.cross_process_interests(
                        probe[0], namespace)
                    # The cross-process half, taken at the SAME point and in
                    # the same non-blocking way (#1436): a probe a foreign
                    # holder conflicts with stays pending rather than
                    # occupying a worker or blocking the dispatch of a
                    # disjoint probe behind it. Taken before `submit`, so no
                    # worker is ever spent on a probe that cannot start.
                    try:
                        hold = probe_resource_lock.acquire(
                            exclusive=lock_exclusive,
                            shared=lock_shared, namespace=namespace,
                            purpose=f"run_probes {probe[0]}")
                    except probe_resource_lock.ResourceBusy as busy:
                        foreign = busy
                        continue
                    ledger.acquire(need_exclusive, need_shared)
                    # Emitted BEFORE the work item is queued (and so before
                    # any engine boots), on the dispatching thread: the
                    # parallel path otherwise says nothing about a probe
                    # until it completes, which is precisely what makes a
                    # timeout here unattributable.
                    progress.begin(diagnostics.attempt_identity(
                        probe[0], probe[1], 1, total_attempts))
                    fut = ex.submit(work, i, probe)
                    running[fut] = (need_exclusive, need_shared, hold)
                    futs.append(fut)
                    pending.remove(item)
                if not running:
                    if foreign is not None:
                        # Reachable only because of a holder OUTSIDE this
                        # runner — another sweep, or a /deflake measurement.
                        # The in-process ledger is idle here, so this is not
                        # the stall below: there is real work pending and it
                        # will become dispatchable when the foreign holder
                        # finishes. Wait for it rather than crashing the
                        # sweep. The wait cannot wedge: an flock dies with
                        # the process holding it, so only a live holder still
                        # doing its work keeps us here.
                        print(f"waiting for {foreign.resource!r} "
                              f"({foreign.interest}) held outside this "
                              f"runner: {foreign.describe()}")
                        time.sleep(resources.RESOURCE_WAIT_POLL)
                        continue
                    # Unreachable: with nothing running the ledger is idle
                    # and no foreign holder was met, so the first pending
                    # probe always dispatches. Say so rather than spin
                    # forever.
                    raise RuntimeError(
                        "probe scheduler stalled with work pending: "
                        f"{[p[0] for _, p in pending]}")
                done_futs, _ = concurrent.futures.wait(
                    running, return_when=concurrent.futures.FIRST_COMPLETED)
                for fut in done_futs:
                    # Released on EVERY outcome — PASS, FAIL and TIMEOUT
                    # alike — and only once `lifecycle.run_one` has
                    # returned, which is
                    # after it reaped the probe's whole process group. A
                    # probe waiting on these resources therefore never starts
                    # while the previous holder's engine is still up. Both
                    # layers are released together, in-process ledger first,
                    # so no window pairs one with the other.
                    probe_exclusive, probe_shared, hold = running.pop(fut)
                    ledger.release(probe_exclusive, probe_shared)
                    hold.release()
                    done += 1
                    key, script, status, elapsed, out, timeout = fut.result()
                    progress.end(
                        diagnostics.attempt_identity(key, script, 1,
                                                     total_attempts),
                        f"{status} ({elapsed:.1f}s)")
                    print(f"[{done}/{n}] {script} ... "
                          f"[timeout {registry.format_timeout(timeout)}] "
                          f"{status} ({elapsed:.1f}s)")
                    results[key] = (script, status, elapsed, out)
        except BaseException:
            # Ctrl-C, or any orchestration failure. cancel() alone cannot
            # stop a work item a free worker already picked up, so `stopping`
            # short-circuits those before they Popen anything; reap_all then
            # takes down the engines already running. Leaving the `with`
            # waits for the in-flight workers to finish their own cleanup.
            groups.stopping.set()
            for pending in futs:
                pending.cancel()
            groups.reap_all()
            # Only AFTER the engines are down: a cross-process holder waiting
            # on these resources must not be let in while this runner's
            # engines are still being torn down.
            for _exclusive, _shared, hold in running.values():
                hold.release()
            raise

    failed = [p for p in chosen if results[p[0]][1] != "PASS"]
    if failed and retries > 0:
        # The parallel batch was already the FIRST attempt, so a probe gets
        # exactly `--retries` more solo attempts here — total attempts
        # (1 + retries) match the sequential path, no bonus try.
        print(f"\nRe-running {len(failed)} failed probe(s) SOLO "
              f"(up to {retries} more attempt(s) each; the parallel "
              f"batch was the first)...")
        for key, script, _ in failed:
            timeout = registry.effective_timeout(key, explicit_timeout)
            for r in range(1, retries + 1):
                # A solo retry is another full probe execution, so it takes
                # the cross-process hold like any other (#1436). The parallel
                # batch was attempt 1, so this is attempt r+1 — and it gets
                # the same before/after pair, so a timeout DURING a retry is
                # attributed exactly as a timeout during the batch is.
                retry_identity = diagnostics.attempt_identity(
                    key, script, r + 1, total_attempts)
                progress.begin(retry_identity, "solo retry")
                with resources.resource_hold(key, namespace, announce=waiting):
                    ok, timed_out, elapsed, out = lifecycle.run_one(
                        script, parallel_base, timeout, groups,
                        hold_env=resources.descendant_hold_env(key, namespace))
                status = "TIMEOUT" if timed_out else ("PASS" if ok else "FAIL")
                progress.end(retry_identity, f"{status} ({elapsed:.1f}s)")
                print(f"  {script} solo retry {r}/{retries} ... "
                      f"[timeout {registry.format_timeout(timeout)}] "
                      f"{status} ({elapsed:.1f}s)")
                results[key] = (script, status, elapsed, out)
                if ok:
                    break

    if tail > 0:
        for key, script, _ in chosen:
            r = results[key]
            if r[1] != "PASS":
                print(f"\n--- {r[0]} ({r[1]}) ---")
                present_failure(r[3], tail)


def summarize(chosen, results, *, wall: float, jobs: int) -> int:
    """The ordered final counts, the FAILED list, and the exit status."""
    n = len(chosen)
    ordered = [(key, results[key][0], results[key][1], results[key][2])
               for key, *_ in chosen]
    passed = sum(1 for _, _, status, _ in ordered if status == "PASS")
    probe_time = sum(elapsed for _, _, _, elapsed in ordered)
    extra = (f" (wall {wall:.1f}s, {probe_time / wall:.1f}x)"
             if jobs > 1 and wall > 0 else "")
    print(f"\n{passed}/{n} passed, total probe-time {probe_time:.1f}s{extra}")
    if passed != n:
        print("FAILED:")
        for key, script, status, _ in ordered:
            if status != "PASS":
                print(f"  {status:8s} {script}")
        return 1
    return 0


def execute(chosen, *, jobs, port, parallel_base, parallel_ports,
            explicit_timeout, retries, tail, namespace, groups, progress,
            wall_start, waiting) -> int:
    """Run the whole validated selection and return the process exit status.

    0 = every selected probe passed, 1 = at least one failed, 130 =
    interrupted after every probe still running, and the engine it booted,
    has been terminated. The invocation is already validated and every
    dependency already constructed by `tools/run_probes.py`; nothing here
    parses an argument or exits a process.
    """
    results: dict[str, tuple[str, str, float, str]] = {}
    total_attempts = retries + 1
    try:
        if jobs <= 1:
            run_sequential(chosen, results, port=port,
                           explicit_timeout=explicit_timeout, retries=retries,
                           tail=tail, namespace=namespace, groups=groups,
                           waiting=waiting)
        else:
            run_parallel(chosen, results, jobs=jobs,
                         parallel_base=parallel_base,
                         parallel_ports=parallel_ports,
                         explicit_timeout=explicit_timeout, retries=retries,
                         tail=tail, namespace=namespace, groups=groups,
                         progress=progress, total_attempts=total_attempts,
                         waiting=waiting)
    except KeyboardInterrupt:
        # Each run_one already reaped its own group on the way out; this is
        # the backstop for a group whose worker never got that far.
        groups.stopping.set()
        groups.reap_all()
        print("\ninterrupted — every probe still running, and the engine it "
              "booted, has been terminated", file=sys.stderr)
        return 130
    return summarize(chosen, results, wall=time.time() - wall_start, jobs=jobs)
