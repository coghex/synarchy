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
2. Take the probe's declared resource interests across processes, waiting no
   longer than the 30 minutes AGENTS.md allows for lock contention.
3. Run probe_flake with one run, its result document at
   `$QURUNTUL_TRIAL_PREFIX.probe-flake.json`, where the adapter's `outcomes`
   hook reads it. Its raw artifacts go under the temp directory — probe_flake
   refuses an artifact root inside a worktree, and the ledger lives under .git —
   with a pointer to them beside the trial.

probe_flake exits 0 for any valid measurement, whatever the checks did.
"""
import hashlib
import os
import sys
import tempfile
import time
from pathlib import Path

LOCK_WAIT_SECONDS = 1800

key = sys.argv[1]
prefix = os.environ["QURUNTUL_TRIAL_PREFIX"]
root = Path(__file__).resolve().parents[1]
os.chdir(root)
sys.path.insert(0, str(root / "tools"))

import probe_engine  # noqa: E402
import probe_flake  # noqa: E402
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

artifacts = Path(tempfile.gettempdir()) / "quruntul-synarchy" / hashlib.sha256(prefix.encode()).hexdigest()[:16]
artifacts.mkdir(parents=True, exist_ok=True)
Path(prefix + ".probe-artifacts.path").write_text(str(artifacts) + "\n")

hold = probe_resource_lock.wait_acquire(
    exclusive=probe_runner_resources.exclusive_resources(key),
    shared=probe_runner_resources.shared_resources(key),
    namespace=namespace, purpose=f"quruntul {key}", deadline=time.monotonic() + LOCK_WAIT_SECONDS)
try:
    code = probe_flake.main(["--probe", key, "--runs", "1", "--result", prefix + ".probe-flake.json",
                             "--artifact-root", str(artifacts)])
finally:
    hold.release()
raise SystemExit(code)
