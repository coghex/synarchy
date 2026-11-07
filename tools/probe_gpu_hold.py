#!/usr/bin/env python3
"""The host-wide `gpu-desktop` hold for a probe that launches its own engine (#2932).

`tools/run_probes.py`, `tools/probe_flake.py`, `tools/deflake.py` and the
quruntul trial take `probe_runner_resources.GPU_RESOURCE` around a `needs-gpu`
probe from the registry's classification. A probe run by hand
(`python3 tools/<name>_probe.py`) goes through none of them, and a few boot a
graphical engine without being classified at all (`wire_lighting_probe.py`,
`scenario_entrypoint_probe.py --offscreen`). So the probe itself takes the
hold, at the one place that launches a graphical engine: `probelib.boot` and
the scenario entry-point probe's own launcher call `ensure_for_launch` with the
engine arguments they are about to exec.

The decision is made from the EFFECTIVE launch arguments, not from the probe's
name: a registry key or a flag in prose says nothing about what actually boots.
`engine_uses_gpu` applies the engine's own boot-profile precedence
(`app/CLAUDE.md`: `--language-report` > `--dump` > `--preview` > `--offscreen` >
`--headless` > graphical), so `--headless` and `--dump` runs never take it and a
probe that merely mentions a graphical flag on a path that fails before any
window or device (`tools/debug_console_boot_probe.py`, `tools/preview_cli_probe.py`)
launches through `probe_engine.engine_command`, which this module does not touch.

One hold per PROCESS, kept until the process exits. A probe may boot several
engines, in sequence or at once (`offscreen_probe.py` runs two together); each
call after the first finds the hold already taken, and the hold outlives every
engine the probe started because it is only dropped by the interpreter's exit
(or by the kernel, if the probe is killed), which is after the probe's own
teardown. A probe launched by something that already holds the resource finds it
in `SYNARCHY_PROBE_HELD_HOST` and takes nothing.

Waiting is unbounded and prints who holds the resource and since when. The wait
happens before the engine is launched, so the probe's READY allowance never pays
for it, and a probe waiting here is waiting, not failing: the runner's probe
timeout does not start until the probe is running, and a probe run by hand has no
timeout of its own to expire.

Lock order. `gpu-desktop` is the only resource a probe can WAIT for while it
holds nothing else. `probelib.boot` prepares the engine (which may wait for the
repository's `cabal-build`) before calling here, a runner takes the whole
interest set in one all-or-nothing attempt, and `/deflake`, `probe_flake` and the
quruntul trial take `gpu-desktop` before any repository interest. A waiter of
this module therefore never holds a repository resource while it queues, which
is what keeps the two scopes from forming a cycle.
"""
from __future__ import annotations

import atexit
import contextlib
import sys

import probe_resource_lock
import probe_runner_resources as resources

#: How often a waiter retries, matching the runner's resource poll.
POLL_SECONDS = resources.RESOURCE_WAIT_POLL

# The profile selectors, in the engine's own precedence order.
_NO_GPU_FIRST = ("--language-report", "--dump")
_GPU_SELECTORS = ("--preview", "--offscreen")
_HEADLESS = "--headless"

_held = None  # the ResourceHold this process took, if it took one


def _has_flag(argv, flag: str) -> bool:
    """True when `flag` is an argument, bare or as `flag=value` (`--dump=terrain`)."""
    return any(arg == flag or arg.startswith(flag + "=") for arg in argv)


def engine_uses_gpu(argv) -> bool:
    """True when an engine launched with `argv` creates a window or a Vulkan device.

    `argv` is the engine's own arguments (a leading executable is harmless: it
    matches no flag). Follows the engine's precedence, so `--dump` beats
    `--offscreen` and `--offscreen` beats `--headless`; no selector at all is the
    graphical default.
    """
    argv = [str(arg) for arg in argv]
    if any(_has_flag(argv, flag) for flag in _NO_GPU_FIRST):
        return False
    if any(_has_flag(argv, flag) for flag in _GPU_SELECTORS):
        return True
    return not _has_flag(argv, _HEADLESS)


def holding() -> bool:
    """True when this process, or an ancestor, holds the host-wide GPU resource."""
    return (_held is not None and not _held.released) or (
        resources.GPU_RESOURCE in resources.inherited_host_resources())


def release() -> None:
    """Drop this process's hold. Idempotent; the interpreter's exit calls it too."""
    global _held
    held, _held = _held, None
    if held is not None:
        held.release()


def _announce(label: str):
    def say(busy) -> None:
        print(f"[{label}] waiting for {resources.GPU_RESOURCE!r}, held outside "
              f"this probe: {busy.describe()}", file=sys.stderr, flush=True)
    return say


def acquire(label: str = "probe", *, namespace=None, root=None, announce=None,
            sleep=None, poll: float | None = None, deadline=None):
    """Take `gpu-desktop` for this process unless it is already covered; wait for it.

    Returns the hold this call took, or None when the process (or an ancestor)
    already holds it. `deadline` is an absolute `time.monotonic()` instant for a
    caller that must give up (a lab wrapper); a probe passes none and waits as
    long as the holder lives, which cannot wedge because an flock dies with the
    process holding it.
    """
    global _held
    if holding():
        return None
    token = namespace if namespace is not None else resources.resource_namespace()
    kwargs = {}
    if sleep is not None:
        kwargs["sleep"] = sleep
    _held = probe_resource_lock.wait_acquire(
        exclusive={resources.GPU_RESOURCE}, namespace=token, root=root,
        purpose=f"{label} ({sys.argv[0] if sys.argv and sys.argv[0] else 'probe'})",
        poll=POLL_SECONDS if poll is None else poll,
        announce=announce if announce is not None else _announce(label),
        deadline=deadline, **kwargs)
    atexit.register(release)
    return _held


@contextlib.contextmanager
def hold_for_probe(key: str, *, namespace=None, root=None, announce=None,
                   sleep=None, poll: float | None = None, deadline=None,
                   environ=None):
    """Hold `gpu-desktop` around ONE run of registered probe `key`, if it needs it.

    The layers that run a registered probe as a child (`probe_flake.measure`,
    the quruntul trial) use this instead of the launch guard above, because the
    registry already says whether the probe is `needs-gpu` and the hold must
    start BEFORE the child does and end after the child's process group is
    reaped. It yields the environment the child must be launched with
    (`SYNARCHY_PROBE_HELD_HOST`), also exported to this process for the
    duration so a harness nested between the two sees the same hold.

    A probe that is not `needs-gpu` yields an empty environment and takes
    nothing; so does one whose ancestor already holds the resource, which
    yields the inherited marker. The wait is unbounded unless `deadline` is
    given, happens before anything is launched, and is outside every probe
    timeout and measured elapsed time.

    Take this BEFORE any repository-scoped interest or port lease and never
    while holding one (module docstring, "Lock order").
    """
    env = resources.os.environ if environ is None else environ
    if key not in resources.gpu_keys():
        yield {}
        return
    if resources.GPU_RESOURCE in resources.inherited_host_resources(env):
        yield resources.host_hold_env(key, env)
        return
    token = namespace if namespace is not None else resources.resource_namespace()
    kwargs = {}
    if sleep is not None:
        kwargs["sleep"] = sleep
    hold = probe_resource_lock.wait_acquire(
        exclusive={resources.GPU_RESOURCE}, namespace=token, root=root,
        purpose=f"{sys.argv[0] if sys.argv and sys.argv[0] else 'probe'} {key}",
        poll=POLL_SECONDS if poll is None else poll,
        announce=announce if announce is not None else _announce(key),
        deadline=deadline, **kwargs)
    try:
        with resources.exported_host_hold(hold, env):
            yield resources.host_hold_env(key, env)
    finally:
        hold.release()


def ensure_for_launch(argv, label: str = "engine", **kwargs):
    """Hold `gpu-desktop` for this process if an engine launched with `argv` needs the GPU.

    Call it after the engine executable is prepared and before it is exec'd.
    Headless and `--dump` launches return None without touching a lock; so does
    a launch the process or an ancestor already covers.
    """
    if not engine_uses_gpu(argv):
        return None
    try:
        return acquire(label, **kwargs)
    except probe_resource_lock.ResourceLockError as error:
        # Exclusion cannot be established, and booting a second graphical engine
        # beside someone else's is the defect this lock exists to prevent.
        sys.exit(f"{label}: the {resources.GPU_RESOURCE!r} resource could not "
                 f"be taken ({error}); refusing to open a window or GPU device "
                 f"without it")


__all__ = ["engine_uses_gpu", "ensure_for_launch", "acquire", "release",
           "holding", "hold_for_probe"]
