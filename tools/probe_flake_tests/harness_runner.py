#!/usr/bin/env python3
"""The `run_one` interface the harness shares with the probe runner (#2087).
"""
from __future__ import annotations

import contextlib
import json
import os
import shutil
import tempfile
import textwrap
import threading
import time
import uuid
from pathlib import Path

from .support import ci_probes, probe_protocol, probe_runner_lifecycle
from .support import SyntheticTree, TOOLS_DIR, expect, run_synthetic

import probe_resource_lock  # type: ignore  # noqa: E402
import probe_runner_resources  # type: ignore  # noqa: E402

def test_run_one_defaults() -> None:
    print("\n-- run_one's extended interface --")
    expect(probe_runner_lifecycle.probe_protocol_env() == {},
           "no protocol wiring produces no environment override")
    env = probe_runner_lifecycle.probe_protocol_env(
        event_path="/e", artifact_dir="/a", engine_log_dir="/l", rts_caps=4)
    expect(env == {probe_protocol.ENV_EVENTS: "/e",
                   probe_protocol.ENV_ARTIFACT_DIR: "/a",
                   probe_protocol.ENV_ENGINE_LOG_DIR: "/l",
                   probe_protocol.ENV_RTS_CAPS: "4"},
           "every protocol parameter reaches the child through the environment")
    # An operator's stale export must not push an ordinary run_probes.py
    # run into protocol mode.
    stale = "/tmp/should-be-ignored.jsonl"
    saved = os.environ.get(probe_protocol.ENV_EVENTS)
    os.environ[probe_protocol.ENV_EVENTS] = stale
    try:
        with SyntheticTree() as tree:
            script = tree.root / "tools" / "echoenv_probe.py"
            script.write_text(textwrap.dedent(f'''\
                import os, sys
                sys.path.insert(0, {TOOLS_DIR!r})
                import probe_protocol
                print(repr(os.environ.get(probe_protocol.ENV_EVENTS)))
            '''), encoding="utf-8")
            _ok, _t, _e, out = probe_runner_lifecycle.run_one("echoenv_probe.py", None, 60.0)
            expect(out.strip() == "None",
                   f"an inherited SYNARCHY_PROBE_EVENTS is stripped from an "
                   f"ordinary run (got {out.strip()!r})")
            _ok, _t, _e, out = probe_runner_lifecycle.run_one(
                "echoenv_probe.py", None, 60.0, event_path="/tmp/wanted.jsonl")
            expect(out.strip() == "'/tmp/wanted.jsonl'",
                   f"the harness's own event path wins (got {out.strip()!r})")
    finally:
        if saved is None:
            os.environ.pop(probe_protocol.ENV_EVENTS, None)
        else:
            os.environ[probe_protocol.ENV_EVENTS] = saved

    import inspect
    signature = inspect.signature(probe_runner_lifecycle.run_one)
    positional = [n for n, p in signature.parameters.items()
                  if p.kind is inspect.Parameter.POSITIONAL_OR_KEYWORD]
    expect(positional == ["script", "port", "timeout", "groups"],
           "run_one's positional interface is unchanged for existing callers")
    expect(all(signature.parameters[n].default is None
               for n in ("event_path", "artifact_dir", "engine_log_dir",
                         "rts_caps")),
           "every new parameter is keyword-only with a default")


# --------------------------------------------------------------------------
# The machine-wide `gpu-desktop` hold around a measured run (#2932)
#
# `probe_flake.measure` is the protocol path's only harness: `/deflake`, the
# quruntul trial and a direct `probe_flake.py --probe` all end in it. The
# synthetic probe below is CLASSIFIED `needs-gpu` for the case, so the
# classification-driven path is exercised without an engine, a GPU or the
# real machine's lock.
# --------------------------------------------------------------------------
@contextlib.contextmanager
def gpu_classified(*keys: str):
    """A private lock root; these synthetic keys are `needs-gpu` while inside."""
    root = Path(tempfile.mkdtemp(prefix="probe-flake-gpu-"))
    root.chmod(probe_resource_lock.SHARED_DIR_MODE)
    saved = (probe_resource_lock.LOCK_ROOT, ci_probes.MANUAL_ONLY_REASONS,
             probe_runner_resources.RESOURCE_NAMESPACE,
             os.environ.get(probe_runner_resources.ENV_HELD_HOST))
    probe_resource_lock.LOCK_ROOT = root
    ci_probes.MANUAL_ONLY_REASONS = dict(
        saved[1], **{key: (ci_probes.Reason(ci_probes.NEEDS_GPU, "synthetic"),)
                     for key in keys})
    probe_runner_resources.RESOURCE_NAMESPACE = f"selftest{uuid.uuid4().hex[:12]}"
    os.environ.pop(probe_runner_resources.ENV_HELD_HOST, None)
    try:
        yield root
    finally:
        (probe_resource_lock.LOCK_ROOT, ci_probes.MANUAL_ONLY_REASONS,
         probe_runner_resources.RESOURCE_NAMESPACE, previous) = saved
        if previous is None:
            os.environ.pop(probe_runner_resources.ENV_HELD_HOST, None)
        else:
            os.environ[probe_runner_resources.ENV_HELD_HOST] = previous
        shutil.rmtree(root, ignore_errors=True)


def measured_env(tree: SyntheticTree, root: Path, key: str) -> dict:
    """Run `key` once under `measure`; what the probe saw about the GPU hold."""
    dump = tree.root / f"{key}.env.json"
    os.environ["SYNTHETIC_ENV_DUMP"] = str(dump)
    os.environ["SYNTHETIC_LOCK_ROOT"] = str(root)
    try:
        measurement = run_synthetic(tree, "pass", probe=key)
    finally:
        os.environ.pop("SYNTHETIC_ENV_DUMP", None)
        os.environ.pop("SYNTHETIC_LOCK_ROOT", None)
    seen = json.loads(dump.read_text()) if dump.exists() else {}
    seen["status"] = measurement.status
    seen["elapsed"] = (measurement.runs[0].elapsed if measurement.runs else None)
    return seen


def test_measure_holds_gpu_desktop_for_a_needs_gpu_probe() -> None:
    print("\n-- measure holds gpu-desktop for a needs-gpu probe, and only for it --")
    gpu = probe_resource_lock.GPU_DESKTOP
    with SyntheticTree(keys=("gpusynth", "headsynth")) as tree, \
            gpu_classified("gpusynth") as root:
        seen = measured_env(tree, root, "gpusynth")
        expect(seen.get("status") == "ok", f"the measurement completes ({seen})")
        expect(seen.get("held_host") == gpu,
               f"the probe is told the host-wide hold is taken ({seen.get('held_host')!r})")
        expect(seen.get("gpu_refused") is True,
               "and it really is held while the probe runs: another checkout is refused")
        free = probe_resource_lock.acquire(
            exclusive={gpu}, namespace="afterwards", purpose="after the run")
        expect(free is not None, "the run's end gives it back")
        free.release()

        seen = measured_env(tree, root, "headsynth")
        expect(seen.get("held_host") is None and seen.get("gpu_refused") is False,
               f"a probe that is not needs-gpu holds nothing and is told nothing ({seen})")

        # An ancestor that already holds it (/deflake, a quruntul trial) is
        # inherited: measure neither queues behind it nor takes a second hold.
        ancestor = probe_resource_lock.acquire(
            exclusive={gpu}, namespace="ancestor", purpose="the ancestor")
        os.environ[probe_runner_resources.ENV_HELD_HOST] = gpu
        try:
            started = time.monotonic()
            seen = measured_env(tree, root, "gpusynth")
        finally:
            os.environ.pop(probe_runner_resources.ENV_HELD_HOST, None)
            ancestor.release()
        expect(seen.get("status") == "ok" and seen.get("held_host") == gpu
               and time.monotonic() - started < 60,
               f"a measurement under an ancestor's hold runs instead of waiting ({seen})")


def test_a_gpu_wait_is_outside_the_measured_run() -> None:
    print("\n-- waiting for gpu-desktop is neither the run's elapsed time nor its failure --")
    gpu = probe_resource_lock.GPU_DESKTOP
    with SyntheticTree(keys=("gpusynth",)) as tree, \
            gpu_classified("gpusynth") as root:
        other = probe_resource_lock.acquire(
            exclusive={gpu}, namespace="othercheckout",
            purpose="another checkout's probe")
        out: dict = {}

        def measure() -> None:
            out.update(measured_env(tree, root, "gpusynth"))

        thread = threading.Thread(target=measure, daemon=True)
        import probe_gpu_hold  # type: ignore
        saved_gpu_poll = probe_gpu_hold.POLL_SECONDS
        probe_gpu_hold.POLL_SECONDS = 0.1
        try:
            thread.start()
            time.sleep(3.0)
            expect(thread.is_alive() and not (tree.root / "gpusynth.env.json").exists(),
                   "the probe is not started while another checkout holds the GPU")
            other.release()
            thread.join(timeout=90)
        finally:
            probe_gpu_hold.POLL_SECONDS = saved_gpu_poll
            other.release()
        expect(not thread.is_alive() and out.get("status") == "ok",
               f"it runs once released and the measurement is valid ({out})")
        expect(out.get("elapsed") is not None and out["elapsed"] < 3.0,
               f"and the 3 s wait is not in the run's elapsed time ({out.get('elapsed')})")


TESTS = (
    test_run_one_defaults,
    test_measure_holds_gpu_desktop_for_a_needs_gpu_probe,
    test_a_gpu_wait_is_outside_the_measured_run,
)
