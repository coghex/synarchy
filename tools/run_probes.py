#!/usr/bin/env python3
"""Opt-in aggregate runner for the headless behavior probes (#370).

Each `tools/*_probe.py` script is a self-contained regression harness: it
boots its own headless engine (`--headless --port NNNN`), drives a scenario
over the debug-console TCP protocol, asserts on the result, and exits 0/1.
They're normally run one at a time, by hand, whichever is relevant to a
change. This script runs a selection of them back-to-back and prints a
single PASS/FAIL summary.

Probes each own their engine (boot + teardown). By default they run one at
a time; `--jobs N` runs up to N CONCURRENTLY, each an independent engine on
its own RESERVED PORT SPAN (#531, #1571) — a probe that binds more than one
port declares how many in `probe_runner_registry.PROBE_PORT_SPANS`, and the
allocator lays the spans end to end so no two concurrent probes overlap.
Which probe starts next is decided LONGEST-EXPECTED-FIRST (#2275), reading
the checked-in `probe_runner_registry.PROBE_EXPECTED_SECONDS`: a probe with
no declared expectation sorts behind every probe that has one, and probes
whose expectations tie keep their registry order. That is the only thing
the expectations decide — `--jobs 1`, `--list`, the port allocation, the
solo retries and the final summary all stay in registry order.
Probes canNOT share a single engine — 8 neutralise the global
`unit_ai.update`, 37 load defs engine-wide, many reuse the same world/page
names, and 16 restart the engine, so there is no clean per-scenario
isolation on one long-lived engine; running independent engines in parallel
gets the speed without that problem. Separate processes and separate ports
are ALL that isolates — every probe still drives the same checkout — so
shared repository-relative resources are scheduled reader/writer (#1322,
#1444): every probe holds the resources named in
`probe_runner_resources.IMPLICIT_SHARED_RESOURCES` in a SHARED interest,
and the few probes that need one to themselves declare it in that module's
`EXCLUSIVE_RESOURCES`, which holds such a probe back until nothing else is
running and holds every other probe back until it has finished.

The build directory is one of those resources (#1570). Probes used to
launch their engine as `cabal run exe:synarchy`, so a `--jobs N` sweep put
N concurrent Cabal processes on one `dist-newstyle` and an otherwise
healthy probe died on the inplace package database before its engine
started. This runner therefore resolves the executable ONCE — one
freshness build plus one `cabal list-bin`, in
`probe_runner_resources.engine_preflight`, after selection is validated
and before any probe is spawned — and hands every probe that absolute path
through the environment, so no probe process invokes Cabal while another
is running. That preflight is itself
a Cabal writer, so it runs inside an EXCLUSIVE `cabal-build` hold: two
aggregate runs cannot build at once, and neither can a build and another
runner's `cabal repl` probe. The few probes that
legitimately still drive Cabal (GHCi consumers: `cabal repl` behind
`persistence_snapshot`/`save_compat_audit`) declare `cabal-build`
EXCLUSIVELY instead, which is the same scheduling mechanism keeping them
off everyone else's toes. Since #1436 the SAME two tables are also
enforced ACROSS processes, through `tools/probe_resource_lock.py`, so a
`/deflake` measurement or a second runner cannot overlap what this one is
holding either. A full sequential run is low tens of
minutes; `--jobs` cuts wall-time to ~total/N (bounded by the slowest single
probe). This is NOT part of any default test tier (see CLAUDE.md Testing
Tiers) — run it deliberately, and prefer `--only`.

Usage:
  python3 tools/run_probes.py                  # run everything, sequentially
  python3 tools/run_probes.py --jobs 4         # up to 4 probes at once
  python3 tools/run_probes.py --only combat,movement
  python3 tools/run_probes.py --list
  python3 tools/run_probes.py --port 9500       # override every probe's --port
  python3 tools/run_probes.py --jobs 4 --port 9500   # ... and base the spans there
  python3 tools/run_probes.py --timeout 300

Probes are launched into their own session, and this runner reaps that
process group after EVERY completion path — not just the timeout (#1323).
A probe that dies of an unexpected exception after booting its engine
never reaches its own teardown, and the engine then outlives it holding
the probe's port; see `probe_runner_lifecycle.reap_group`.

Exit 0 = all selected probes passed. 1 = at least one failed. 2 = the run
never started — a bad invocation (e.g. --only matched nothing), an
unusable cross-process resource namespace, or an engine executable the
preflight could not resolve. 130 = interrupted with Ctrl-C, after
terminating every probe still running and the engine it booted.

This module is the COMMAND and nothing else (#2074). The implementation
lives in five owners beside it, each of which is importable on its own:

  `probe_runner_registry`     the probe list, selection, port spans,
                              per-key timeout and expected-duration
                              declarations, and the dispatch order the
                              latter decides
  `probe_runner_diagnostics`  the durable progress (#1768) and failure
                              (#1982) record protocols
  `probe_runner_resources`    the reader/writer conflict model, the
                              cross-process holds, the inherited ancestor
                              holds, and the engine preflight
  `probe_runner_lifecycle`    launching one probe and reaping its whole
                              process group (#1323)
  `probe_runner_scheduler`    sequential and `--jobs` orchestration,
                              retries, presentation and the summary

Dependencies run one way — registry and diagnostics are leaves, resources
and lifecycle build on them, the scheduler builds on all four, and this
file builds on the scheduler. A facility has exactly ONE owner and this
file re-exports none of them: a tool or test that reaches for `PROBES`,
`run_one`, `ENGINE_EXECUTABLE` or a record parser imports the owner, so
assigning to it changes the state the implementation actually reads.
"""
from __future__ import annotations
import argparse
import math
import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_engine  # noqa: E402
import probe_resource_lock  # noqa: E402
import probe_runner_diagnostics as diagnostics  # noqa: E402
import probe_runner_lifecycle as lifecycle  # noqa: E402
import probe_runner_registry as registry  # noqa: E402
import probe_runner_resources as resources  # noqa: E402
import probe_runner_scheduler as scheduler  # noqa: E402


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--only", default=None,
                     help="comma-separated substrings matched against probe key/filename")
    ap.add_argument("--exact", action="store_true",
                     help="treat --only as exact probe KEYS, not substrings (the CI gate "
                          "uses this so e.g. 'craft' can't also select 'craft_bill')")
    ap.add_argument("--list", action="store_true", help="list known probes and exit")
    ap.add_argument("--port", type=int, default=None,
                     help="base port. Sequentially it overrides every probe's "
                          "--port; with --jobs it is the ORIGIN the "
                          "non-overlapping per-probe spans are laid out from, "
                          "instead of the default 9400 (#1571). Unset, each "
                          "probe keeps its own default sequentially and the "
                          "parallel allocation starts at 9400. Never 8008, and "
                          "never a base whose span reaches it.")
    ap.add_argument(
        "--timeout", type=float, default=None,
        help=("explicit wall-clock timeout in seconds for every selected "
              f"probe (ordinary default {registry.DEFAULT_TIMEOUT:g}; registered "
              "per-key defaults apply when omitted)"))
    ap.add_argument("--tail", type=int, default=25,
                     help="lines of captured output to print for a failing probe")
    ap.add_argument("--retries", type=int, default=0,
                     help="on failure, re-run a probe SOLO up to N more times before "
                          "marking it failed — absorbs the sequential-engine contention "
                          "flakes seen in a back-to-back run (#530); a probe that passes "
                          "on any attempt counts as PASS")
    ap.add_argument("--jobs", type=int, default=1, metavar="N",
                     help="run up to N probes CONCURRENTLY, each its own engine on its "
                          "own reserved port span (#531, #1571), dispatched "
                          "LONGEST-EXPECTED-FIRST from the checked-in "
                          "probe_runner_registry.PROBE_EXPECTED_SECONDS — undeclared "
                          "probes last, ties in registry order (#2275). Cuts wall-time to "
                          "~total/N. Default 1 = the "
                          "sequential behavior CI relies on, which keeps registry order. "
                          "Since concurrency raises "
                          "contention, --retries re-runs failures SOLO after the parallel "
                          "batch. Cap N to (cores - 1) or so — each probe is a full engine.")
    args = ap.parse_args()

    if (args.timeout is not None
            and (not math.isfinite(args.timeout) or args.timeout <= 0)):
        print(f"--timeout must be finite and positive (got {args.timeout!r})",
              file=sys.stderr)
        return 2

    timeout_problems = registry.timeout_override_problems()
    if timeout_problems:
        print("registered probe timeout declarations are unusable:",
              file=sys.stderr)
        for problem in timeout_problems:
            print(f"  - {problem}", file=sys.stderr)
        return 2

    if args.port == registry.GUI_PORT:
        # The base itself. Refused here, before `--list` and before any
        # selection, so the obvious mistake never depends on which probes
        # were chosen. The SPAN-aware refusal below needs the selection
        # and therefore runs after it.
        sys.exit(f"refusing --port {registry.GUI_PORT}: that's the user's GUI port, "
                 f"see CLAUDE.md")

    chosen = registry.select(args.only, exact=args.exact)
    if args.exact:
        # A MIXED valid/invalid request is reported by naming the unknown
        # keys, before any listing or running. An ALL-invalid request
        # falls through to the empty-selection branch below unchanged
        # (`chosen` is empty there too, so this never fires for it).
        unknown = registry.unknown_exact_keys(args.only)
        if unknown and chosen:
            print(f"--only {args.only!r} names unknown probe key(s) with "
                  f"--exact: {', '.join(unknown)}; see --list", file=sys.stderr)
            return 2
    if not chosen:
        print(f"--only {args.only!r} matched no probes; see --list", file=sys.stderr)
        return 2

    if args.list:
        for key, script, purpose in chosen:
            print(f"{key:28s} {script:32s} {purpose}")
        return 0

    # The WHOLE port plan is computed and validated here, before a single
    # subprocess exists (#1571): every port any selected probe may bind,
    # in the mode it is about to run in. A probe that would reach the GUI
    # port is a refusal, not a boot against the user's running game.
    parallel_base = registry.PARALLEL_PORT_BASE if args.port is None else args.port
    parallel_ports = registry.allocate_parallel_ports(chosen, parallel_base)
    if args.jobs <= 1:
        # Sequential hands every probe the same base, one at a time, so
        # there is nothing to lay out — but `--port 8007` still reaches
        # 8008 through a two-port probe's span.
        planned = ([] if args.port is None
                   else [(key, args.port) for key, _, _ in chosen])
    else:
        planned = [(key, port)
                   for (key, _, _), port in zip(chosen, parallel_ports)]
        if args.retries > 0:
            # A parallel failure is re-run SOLO from the allocation
            # origin, so that span is part of the plan too.
            planned += [(key, parallel_base) for key, _, _ in chosen]
    conflicts = registry.gui_port_conflicts(planned)
    if conflicts:
        print(registry.describe_gui_conflicts(conflicts), file=sys.stderr)
        return 2

    # Resolved ONCE, here: after --list has had its chance to return without
    # needing a repository, and before any probe runs, so an unusable
    # namespace is a loud refusal in the first second rather than a sweep
    # that silently coordinates with nobody. It precedes the executable
    # preflight below for exactly that reason — the preflight may spend a
    # build, and a namespace this run can never use should not wait behind
    # one.
    try:
        namespace = resources.resource_namespace()
    except probe_resource_lock.ResourceLockError as error:
        print(f"cannot coordinate probe resources across processes: {error}",
              file=sys.stderr)
        return 2

    def waiting(busy) -> None:
        print(f"\n    waiting for {busy.resource!r} ({busy.interest}) held "
              f"outside this runner ... ", end="", flush=True)

    # The whole Cabal contact this run makes (#1570): one freshness build
    # plus one `cabal list-bin`, HERE — after `--list` and after every
    # selection and port refusal above, so a rejected or empty selection
    # stays build-free, and before a single probe process exists, so the
    # concurrent-`cabal run` race cannot happen at all. The build itself
    # runs inside an EXCLUSIVE `cabal-build` hold, because a preflight is
    # a Cabal writer like any other and a second runner's preflight (or
    # another runner's `cabal repl` probe) must not be in the build
    # directory beside it. A failure is this runner's own nonzero exit,
    # never a retry and never a probe's assertion failure. The resolved
    # path is stored on its OWNER, which is the cell
    # `probe_runner_lifecycle.run_one` reads when it hands a child its
    # engine (#2074) — this file keeps no copy of it.
    try:
        resources.ENGINE_EXECUTABLE = resources.engine_preflight(
            namespace, announce=waiting)
    except probe_engine.EngineExecutableError as error:
        print(f"cannot resolve the engine the probes launch: {error}",
              file=sys.stderr)
        return 2
    except probe_resource_lock.ResourceLockError as error:
        print(f"cannot coordinate the engine build across processes: {error}",
              file=sys.stderr)
        return 2
    print(f"engine: {resources.ENGINE_EXECUTABLE}")

    wall_start = time.time()
    # #1768: the runner's own half of the shared progress convention. Its
    # records go to this process's stdout, which — when this runner is
    # NESTED inside a probe (the persistence sweep runs one) — is that
    # probe's captured pipe, so they survive the outer runner's timeout
    # kill and name which attempts were in flight.
    progress = diagnostics.ProgressEmitter(wall_start)
    groups = lifecycle.ProbeGroups()

    return scheduler.execute(
        chosen, jobs=args.jobs, port=args.port, parallel_base=parallel_base,
        parallel_ports=parallel_ports, explicit_timeout=args.timeout,
        retries=args.retries, tail=args.tail, namespace=namespace,
        groups=groups, progress=progress, wall_start=wall_start,
        waiting=waiting)


if __name__ == "__main__":
    sys.exit(main())
