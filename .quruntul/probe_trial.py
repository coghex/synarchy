#!/usr/bin/env python3
"""One quruntul trial of a probe-result/v1 probe: `tools/probe_flake.py --runs 1`, in process.

This does what `tools/deflake.py` does around a measurement, through the public
pieces it is built from:

1. Adopt the engine and save-codec executables the adapter's `prepare` resolved
   once for the batch (`.quruntul/preflight.py`), handed down through the
   variables the runner already reads, and put them in the cells
   `probe_runner_lifecycle.run_one` hands to the probe. The trial builds nothing:
   a probe left to build its own engine prints a bracketed preparation line,
   which probe_flake rightly rejects in protocol mode, and a build here would
   have to fit inside the trial's time limit.
2. For a `needs-gpu` probe, first take the machine-wide `gpu-desktop` hold (#2932)
   and wait for it as long as `GPU_WAIT_SECONDS` allows. It is taken before any
   repository interest, so this trial never queues for the GPU while holding the
   build state, and the adapter adds the same allowance to the trial's limit so
   waiting for another checkout's GPU probe is never charged to this one. Running
   out of that allowance ends the trial as blocked (exit 3, no probe result), not
   as a failed or timed-out probe.
   Then take the probe's declared repository interests across processes, waiting
   no longer than the 30 minutes AGENTS.md allows for lock contention.
3. Run probe_flake with one run, its result document at
   `$QURUNTUL_TRIAL_PREFIX.probe-flake.json`, where the adapter's `outcomes`
   hook reads it. It is given NO `--artifact-root` (#2931): a failing trial's
   evidence goes to the durable failure root (`$SYNARCHY_PROBE_FAILURE_ROOT`,
   else `~/.local/state/synarchy/probe-failures`), which survives a reboot and
   a per-session TMPDIR and is pruned to the newest few failures per probe.
   probe_flake still refuses a root inside a worktree, and the ledger lives
   under .git. A pointer to the kept directory is written beside the trial once
   the run has finished, and only when it kept something.

probe_flake exits 0 for any valid measurement, whatever the checks did.

`probe_trial.py KEY --run-probes PORT` is the same trial for a probe that is not on
the `probe-result/v1` protocol (the `exit` suites): it takes the `gpu-desktop` hold
under the same bounded wait and exit-3 rule, then runs `tools/run_probes.py --only
KEY --exact --jobs 1 --port PORT` beneath it and exits with that runner's status. A
`needs-gpu` exit suite is launched this way so that giving up on the GPU is a blocked
trial, never a probe FAIL or TIMEOUT, whichever framework the probe uses.
"""
import contextlib
import json
import os
import sys
import time
from pathlib import Path

LOCK_WAIT_SECONDS = 1800
GPU_WAIT_SECONDS = 4 * 3600  # keep equal to the adapter's: it extends the trial limit by this much

key = sys.argv[1]
run_probes_port = sys.argv[3] if len(sys.argv) > 3 and sys.argv[2] == "--run-probes" else None
prefix = os.environ.get("QURUNTUL_TRIAL_PREFIX", "")
root = Path(__file__).resolve().parents[1]
os.chdir(root)
sys.path.insert(0, str(root / "tools"))

import subprocess  # noqa: E402

import probe_engine  # noqa: E402
import probe_flake  # noqa: E402
import probe_gpu_hold  # noqa: E402
import probe_resource_lock  # noqa: E402
import probe_runner_resources  # noqa: E402
import save_compat_audit_codec  # noqa: E402

namespace = probe_resource_lock.repository_namespace(root)
announce = lambda message: print(f"quruntul: {message}", flush=True)  # noqa: E731
if run_probes_port is not None:
    # A non-protocol probe: the GPU wait is bounded and blocked-not-failed exactly as
    # below, then the coordinated runner (which builds nothing: the adapter handed it
    # the prepared executables) runs the probe under the exported hold.
    with contextlib.ExitStack() as stack:
        try:
            stack.enter_context(probe_gpu_hold.hold_for_probe(
                key, namespace=namespace, deadline=time.monotonic() + GPU_WAIT_SECONDS))
        except probe_resource_lock.ResourceBusy as busy:
            print(f"quruntul: {busy.describe()}; gave up after {GPU_WAIT_SECONDS} s of waiting. "
                  "This is contention, not a probe result.", flush=True)
            raise SystemExit(3)
        done = subprocess.run([sys.executable, "tools/run_probes.py", "--only", key, "--exact",
                               "--jobs", "1", "--port", run_probes_port])
    raise SystemExit(done.returncode)
if not (probe_engine.runner_executable(os.environ)
        and probe_engine.runner_executable(os.environ, save_compat_audit_codec.ENV_CODEC_EXE)):
    print("quruntul: no prepared engine and codec were handed down; the adapter's prepare did not run", flush=True)
    raise SystemExit(3)
# Adopts the handed-down executables; nothing is built here.
probe_runner_resources.ENGINE_EXECUTABLE = probe_runner_resources.engine_preflight(namespace, announce=announce)
probe_runner_resources.CODEC_EXECUTABLE = probe_runner_resources.codec_preflight(namespace, announce=announce)

with contextlib.ExitStack() as stack:
    try:
        stack.enter_context(probe_gpu_hold.hold_for_probe(
            key, namespace=namespace, deadline=time.monotonic() + GPU_WAIT_SECONDS))
    except probe_resource_lock.ResourceBusy as busy:
        print(f"quruntul: {busy.describe()}; gave up after {GPU_WAIT_SECONDS} s of waiting. "
              "This is contention, not a probe result.", flush=True)
        raise SystemExit(3)
    # The host-wide interest is now held (and exported for probe_flake), so the
    # repository set drops it: only what is still ours to wait for is bounded.
    exclusive, shared = probe_runner_resources.cross_process_interests(key, namespace)
    hold = probe_resource_lock.wait_acquire(
        exclusive=exclusive, shared=shared,
        namespace=namespace, purpose=f"quruntul {key}", deadline=time.monotonic() + LOCK_WAIT_SECONDS)
    try:
        code = probe_flake.main(["--probe", key, "--runs", "1", "--result", prefix + ".probe-flake.json"])
    finally:
        hold.release()
# Where the failure evidence went, if any: the result document names it.
try:
    kept = json.loads(Path(prefix + ".probe-flake.json").read_text()).get("retained_artifacts") or []
except (OSError, ValueError):
    kept = []
if kept:
    Path(prefix + ".probe-artifacts.path").write_text("\n".join(kept) + "\n")
raise SystemExit(code)
