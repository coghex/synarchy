#!/usr/bin/env python3
"""Resolve the engine and save-codec executables once, before a quruntul batch.

The adapter's `prepare` runs this for a probe suite, inside quruntul's build
budget, so a trial never builds and its time limit covers only its resource
wait and the probe. It is `tools/run_probes.py`'s preflight, run on its own:

- `cabal-build` is taken exclusively, waiting no longer than the 30 minutes
  AGENTS.md allows for lock contention, and handed to both preflights through
  the runner's held-resource environment, so neither waits on the lock itself;
- `probe_runner_resources.engine_preflight` and `codec_preflight` build and
  locate the two executables.

The last line of output is `QURUNTUL_EXECUTABLES {json}`, naming them; the
adapter hands them to every trial through the variables the runner and probes
already read (`SYNARCHY_PROBE_ENGINE_EXE`, `SYNARCHY_SAVE_CODEC_EXE`).
"""
import json
import os
import sys
import time
from pathlib import Path

LOCK_WAIT_SECONDS = 1800

root = Path(__file__).resolve().parents[1]
os.chdir(root)
sys.path.insert(0, str(root / "tools"))

import probe_engine  # noqa: E402
import probe_resource_lock  # noqa: E402
import probe_runner_resources  # noqa: E402
import save_compat_audit_codec  # noqa: E402

namespace = probe_resource_lock.repository_namespace(root)
announce = lambda message: print(f"quruntul: {message}", flush=True)  # noqa: E731
build = probe_resource_lock.wait_acquire(
    exclusive=(probe_runner_resources.BUILD_RESOURCE,), namespace=namespace,
    purpose="quruntul preflight", deadline=time.monotonic() + LOCK_WAIT_SECONDS)
try:
    held = dict(os.environ, **{probe_runner_resources.ENV_HELD_NAMESPACE: namespace,
                               probe_runner_resources.ENV_HELD_EXCLUSIVE: probe_runner_resources.BUILD_RESOURCE})
    engine = probe_runner_resources.engine_preflight(namespace, environ=held, announce=announce)
    codec = probe_runner_resources.codec_preflight(namespace, environ=held, announce=announce)
finally:
    build.release()
print("QURUNTUL_EXECUTABLES " + json.dumps({probe_engine.ENV_ENGINE_EXE: engine,
                                            save_compat_audit_codec.ENV_CODEC_EXE: codec}), flush=True)
