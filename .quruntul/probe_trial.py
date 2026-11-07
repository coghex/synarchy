#!/usr/bin/env python3
"""One quruntul trial of a probe: `tools/probe_flake.py --runs 1` (a `probe-result/v1` probe), or
`tools/run_probes.py` (`--run-probes`, a `needs-gpu` probe that is not on that protocol), in process.

This does what `tools/deflake.py` does around a measurement, through the public
pieces it is built from:

1. Adopt the engine and save-codec executables the adapter's `prepare` resolved
   once for the batch (`.quruntul/preflight.py`), handed down through the
   variables the runner already reads, and put them in the cells
   `probe_runner_resources` hands to the probe. The trial builds nothing:
   a probe left to build its own engine prints a bracketed preparation line,
   which probe_flake rightly rejects in protocol mode, and a build here would
   have to fit inside the trial's time limit.
2. Take the probe's WHOLE declared interest set across processes in one atomic
   wait: the repository interests and, for a `needs-gpu` probe, the machine-wide
   `gpu-desktop` hold (#2932) with them. One acquisition, because taking the GPU
   first and the build state afterwards would let a runner queued for both, and a
   build queued behind it, form a cycle through the fairness queue. The hold is
   then forwarded to everything this trial runs, so nothing beneath it asks again.
   A non-GPU probe waits no longer than the 30 minutes AGENTS.md allows for lock
   contention; a `needs-gpu` one waits up to `GPU_WAIT_SECONDS`, which the adapter
   adds to the trial's limit so waiting for another checkout's GPU probe is never
   charged to this one. Running out of that wait ends the trial as BLOCKED
   (exit 3, and no result document, which the lab records as a blocked trial), not
   as a failed or timed-out probe.
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

`probe_trial.py KEY --run-probes PORT` runs `tools/run_probes.py --only KEY --exact
--jobs 1 --port PORT` instead of probe_flake, under the same hold, and records its
exit status in `$QURUNTUL_TRIAL_PREFIX.run-probes.json`. Every registered probe on
the GPU is launched this way unless it is on the protocol.
"""
import json
import os
import subprocess
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

import probe_engine  # noqa: E402
import probe_flake  # noqa: E402
import probe_gpu_hold  # noqa: E402
import probe_resource_lock  # noqa: E402
import probe_runner_resources  # noqa: E402
import save_compat_audit_codec  # noqa: E402

namespace = probe_resource_lock.repository_namespace(root)
announce = lambda message: print(f"quruntul: {message}", flush=True)  # noqa: E731
if not (probe_engine.runner_executable(os.environ)
        and probe_engine.runner_executable(os.environ, save_compat_audit_codec.ENV_CODEC_EXE)):
    print("quruntul: no prepared engine and codec were handed down; the adapter's prepare did not run", flush=True)
    raise SystemExit(3)
# Adopts the handed-down executables; nothing is built here.
probe_runner_resources.ENGINE_EXECUTABLE = probe_runner_resources.engine_preflight(namespace, announce=announce)
probe_runner_resources.CODEC_EXECUTABLE = probe_runner_resources.codec_preflight(namespace, announce=announce)

exclusive, shared = probe_runner_resources.cross_process_interests(key, namespace)


def waiting(busy) -> None:
    """Say what the wait is for and who holds it, while it lasts (#2932)."""
    print(f"quruntul: waiting for {busy.resource!r} ({busy.interest}) held outside this trial: "
          f"{busy.describe()}", flush=True)


wait = GPU_WAIT_SECONDS if probe_runner_resources.GPU_RESOURCE in exclusive else LOCK_WAIT_SECONDS
try:
    hold = probe_resource_lock.wait_acquire(
        exclusive=exclusive, shared=shared, namespace=namespace, purpose=f"quruntul {key}",
        poll=probe_runner_resources.RESOURCE_WAIT_POLL, announce=waiting, deadline=time.monotonic() + wait)
except probe_resource_lock.ResourceBusy as busy:
    print(f"quruntul: {busy.describe()}; gave up after {wait} s of waiting. "
          "This is contention, not a probe result.", flush=True)
    raise SystemExit(3)
try:
    # The host-wide interest is marked for everything beneath this process.
    with probe_runner_resources.exported_host_hold(hold):
        if run_probes_port is not None:
            env = dict(os.environ)
            env.update(probe_runner_resources.descendant_hold_env(key, namespace))
            fds = probe_gpu_hold.child_fds()
            if fds:
                env[probe_runner_resources.ENV_HELD_HOST_FDS] = ",".join(str(fd) for fd in fds)
            done = subprocess.run(
                [sys.executable, "tools/run_probes.py", "--only", key, "--exact", "--jobs", "1",
                 "--port", run_probes_port], env=env, pass_fds=tuple(fds))
            code = done.returncode
            Path(prefix + ".run-probes.json").write_text(json.dumps({"returncode": code}))
        else:
            code = probe_flake.main(["--probe", key, "--runs", "1", "--result", prefix + ".probe-flake.json"])
finally:
    hold.release()
if run_probes_port is not None:
    raise SystemExit(code)
# Where the failure evidence went, if any: the result document names it.
try:
    kept = json.loads(Path(prefix + ".probe-flake.json").read_text()).get("retained_artifacts") or []
except (OSError, ValueError):
    kept = []
if kept:
    Path(prefix + ".probe-artifacts.path").write_text("\n".join(kept) + "\n")
raise SystemExit(code)
