#!/usr/bin/env python3
"""Where a probe's engine executable comes from (#1570).

Every probe used to launch its engine as `cabal run -v0 exe:synarchy --`,
and `tools/run_probes.py --jobs N` runs up to N probes at once against ONE
checkout. Concurrent `cabal run` invocations mutate the same
`dist-newstyle` inplace package database, so an otherwise healthy probe
dies before its engine starts — `package.cache:
removeDirectoryRecursive:fstatat: does not exist`, `ghc-pkg: cannot
create: .../package.conf.inplace already exists`. Reproduced at `--jobs 3`
over five probes; the same five pass one at a time. The failure is the
runner's build state, not the probes, and `--retries` only re-runs the
loser.

This module is the one funnel that removes the race by construction:

* **The runner resolves the executable ONCE**, before any probe process
  starts (`resolve_executable`), and hands it to every probe it launches
  through `SYNARCHY_PROBE_ENGINE_EXE`.
* **A probe launches whatever it was handed** (`engine_command`), so no
  probe process invokes Cabal while another probe process is running.
* **A probe run BY HAND still works with no prior build step**: with no
  runner-supplied executable, `engine_command` falls back to the
  `cabal run` invocation probes have always used, and
  `prepare_executable` (below) does the same job ahead of the caller's
  clock. Requirement 3 of #1570 is that no hand-run probe needs a build
  step of its own, and both spellings keep it.

Direct invocation prepares before it launches (#1913)
-----------------------------------------------------

The fallback above is a BUILD wearing an engine's argv, and
`probelib.boot` used to start its READY deadline the moment that child
existed — so a cold compile was timed as though an engine were already
starting, expired at 180 seconds, reported "engine never printed READY"
against an empty `-v0` log, and killed only the `cabal` process while
its `setup`/GHC descendants kept compiling.

`prepare_executable` below is the direct path's answer, and it is the
aggregate preflight's shape rather than a second design: ONE freshness
build plus ONE `cabal list-bin`, inside an EXCLUSIVE `cabal-build` hold,
finished before the caller launches anything. What the caller then execs
is the absolute binary, so the READY deadline measures engine startup
and nothing else. #1570's requirement 3 is untouched — a probe run by
hand from a clean checkout still needs no prior build step; the build
simply happens somewhere the clock is not already running.

The module imports exactly one other `tools/` module,
`probe_resource_lock`, which imports nothing from `tools/` at all:
`run_probes.py` imports this one for the preflight, `probelib.py`
imports it for `boot`, and the four probes with their own private
launchers import it too — anything reaching back into THOSE would close
an import cycle.
"""
from __future__ import annotations

import contextlib
import os
import signal
import subprocess
import tempfile
import time

import probe_resource_lock

#: This checkout's root, derived from this file's own location, so a
#: directly invoked probe needs no Cabal contact to say where to build.
#: `run_probes.REPO_ROOT` derives the identical value the identical way.
REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

#: The runner -> probe contract. Its value is an ABSOLUTE path to an
#: already-built `exe:synarchy`; its presence is what selects
#: aggregate (prebuilt) mode over the direct-invocation fallback.
ENV_ENGINE_EXE = "SYNARCHY_PROBE_ENGINE_EXE"

#: The direct-invocation fallback: the exact argv prefix every probe used
#: before #1570, kept verbatim so a hand-run probe behaves as it always
#: has. `--` separates cabal's own flags from the engine's.
CABAL_RUN_PREFIX: tuple[str, ...] = ("cabal", "run", "-v0", "exe:synarchy", "--")

#: The Cabal target every probe's engine comes from.
ENGINE_TARGET = "exe:synarchy"

#: The shared Cabal build state — this checkout's one `dist-newstyle`,
#: whose concurrent mutation is the whole of #1570. Named HERE, in the
#: module that owns every Cabal contact a probe makes, so the aggregate
#: preflight, the direct preparation below and `run_probes`'s two
#: declaration tables cannot drift apart. `run_probes.BUILD_RESOURCE`
#: is this name.
BUILD_RESOURCE = "cabal-build"

#: How long direct-invocation preparation gets, end to end: lock
#: acquisition, the freshness build and the `cabal list-bin` together.
#: It matches `tools/playtest/launch.py`'s full-cold-build watchdog,
#: which is this repository's established allowance for the same work.
#: It is DELIBERATELY independent of `probelib.DEFAULT_READY_TIMEOUT`:
#: that one measures an engine starting, and the entire point of #1913
#: is that the two are different things being timed.
DEFAULT_PREPARE_TIMEOUT = 1800.0

#: How often a preparation that is waiting for `cabal-build` retries.
PREPARE_LOCK_POLL = 1.0

#: Grace given to a preparation process group between SIGTERM and
#: SIGKILL, and then to the leader after the SIGKILL.
GROUP_TERM_GRACE = 5.0
GROUP_KILL_GRACE = 5.0

#: Test seam: where preparation's `cabal-build` flock lives. Production
#: leaves it None, which is `probe_resource_lock.LOCK_ROOT` (`/tmp`) —
#: the same root every runner and `/deflake` measurement coordinates in.
#: `tools/test_probelib.py` redirects it to an isolated sticky scratch
#: directory so its cases neither wait on nor block a real run.
PREPARE_LOCK_ROOT = None


class EngineExecutableError(RuntimeError):
    """The engine executable could not be resolved, or is unusable.

    Raised by the runner's preflight and by the validation of a
    runner-supplied path. Both are refusals BEFORE any probe starts, so
    the diagnostic has to name the repair rather than surface later as a
    probe assertion failure.
    """


class EnginePreparationError(EngineExecutableError):
    """PREPARING the executable failed — the engine never got to start.

    Its own type because the diagnostic it produces answers a different
    question from every other engine failure: a build that could not be
    run, could not take the build directory, overran its allowance or
    exited nonzero is not an engine that failed to become ready, and
    reporting it as one is what sent two coordinated runs looking for a
    boot defect in an empty log (#1913).

    It is a subclass, so a caller that already handles
    `EngineExecutableError` keeps catching this too.
    """


def validate_executable(path: str, *, source: str) -> str:
    """Return `path` if it is an absolute, existing, executable file.

    `source` names where the path came from, so the diagnostic points at
    the thing to fix (a stale environment export, or a `cabal list-bin`
    answer that no longer exists) rather than at the probe that tripped
    over it.
    """
    if not path:
        raise EngineExecutableError(f"{source} named no engine executable")
    if not os.path.isabs(path):
        raise EngineExecutableError(
            f"{source} named {path!r}, which is not an absolute path; the "
            f"engine executable is handed to probes that run in other "
            f"working directories, so it has to be absolute")
    if not os.path.isfile(path):
        raise EngineExecutableError(
            f"{source} named {path!r}, which is not an existing file")
    if not os.access(path, os.X_OK):
        raise EngineExecutableError(
            f"{source} named {path!r}, which is not executable")
    return path


def runner_executable(environ=None) -> str | None:
    """The validated executable the aggregate runner resolved, or None.

    None means "nobody handed us one" — a probe invoked directly from the
    repository root — and the caller then reaches Cabal itself, through
    `engine_command`'s `cabal run` fallback or through
    `prepare_executable`'s build. A value that IS present but unusable is
    an error rather than a silent fallback either way: falling back would
    put a Cabal process back inside a parallel sweep, which is the whole
    defect.
    """
    raw = (os.environ if environ is None else environ).get(ENV_ENGINE_EXE)
    if raw is None or not raw.strip():
        return None
    return validate_executable(raw.strip(), source=f"${ENV_ENGINE_EXE}")


def engine_command(args, environ=None) -> list[str]:
    """Argv that runs the engine with `args`, in whichever mode applies.

    Aggregate mode (the runner supplied an executable) execs the absolute
    validated binary directly; direct mode keeps the historical `cabal
    run` invocation. `args` are the engine's own arguments in both cases,
    in the same order — only the prefix differs, so a caller's flags,
    RTS block, working directory and stream handling are unaffected.
    """
    executable = runner_executable(environ)
    if executable is not None:
        return [executable, *args]
    return [*CABAL_RUN_PREFIX, *args]


def _last_line(text: str) -> str:
    """The last non-empty line of `text`, stripped.

    `cabal list-bin` can precede its answer with warnings, so the path is
    the last line rather than the whole of stdout.
    """
    lines = [line.strip() for line in (text or "").splitlines() if line.strip()]
    return lines[-1] if lines else ""


def resolve_executable(repo_root, *, run=None) -> str:
    """Build `exe:synarchy` if stale, then return its absolute path.

    ONE freshness build plus ONE read-only path query — the whole Cabal
    contact an aggregate run is allowed, and it happens before any probe
    process exists. The build is unconditional on purpose: `cabal
    list-bin` answers with a path whether or not that file is current, so
    "build only when the file is missing" (which is all
    `resource_root_probe.locate_binary` ever did) would happily hand a
    sweep a stale engine.

    `run` is the subprocess entry point, injectable so
    `tools/test_run_probes.py` can prove the preflight's shape — one
    build, one query, before any probe spawns — without a toolchain.
    """
    runner = subprocess.run if run is None else run
    for step, argv in (("build", ["cabal", "build", ENGINE_TARGET]),
                       ("locate", ["cabal", "list-bin", ENGINE_TARGET])):
        try:
            done = runner(argv, cwd=str(repo_root), capture_output=True,
                          text=True)
        except FileNotFoundError:
            raise EngineExecutableError(
                f"'cabal' was not found on PATH, so {ENGINE_TARGET} cannot "
                f"be resolved for the probes") from None
        except OSError as error:
            raise EngineExecutableError(
                f"could not run {' '.join(argv)}: {error}") from None
        if done.returncode != 0:
            tail = _last_line(done.stderr) or _last_line(done.stdout)
            raise EngineExecutableError(
                f"`{' '.join(argv)}` failed with exit status "
                f"{done.returncode}"
                + (f": {tail}" if tail else "")
                + f"; the probes need a buildable {ENGINE_TARGET}")
        if step == "locate":
            return validate_executable(
                _last_line(done.stdout),
                source=f"`cabal list-bin {ENGINE_TARGET}`")
    raise AssertionError("unreachable")  # pragma: no cover


# ---------------------------------------------------------------------------
# Direct-invocation preparation (#1913)
#
# Everything below runs ONLY when nobody handed this process an
# executable. Under the aggregate runner `prepare_executable` returns the
# inherited path without touching Cabal, the lock, or a log file, so a
# probe the runner launched still makes no Cabal contact of its own.


def _signal_group(pgid: int, sig: int) -> None:
    """Deliver `sig` to the whole process group, ignoring its absence.

    A group with no members left raises `ProcessLookupError`, which is
    success here rather than a problem: there was nothing to dispose of.
    Every other `OSError` is swallowed for the same reason — this runs
    on a failure path that already has a diagnostic to deliver.
    """
    with contextlib.suppress(OSError):
        os.killpg(pgid, sig)


def _reap_group(proc: subprocess.Popen, pgid: int) -> None:
    """Dispose of the entire process group `proc` leads.

    `cabal` is not a leaf: it spawns `setup` and GHC, and killing the
    leader alone leaves them compiling into the build directory this
    preparation was supposed to be holding exclusively — which is
    exactly what a coordinated run observed still running after the
    probe that started it had exited (#1913).

    The SIGKILL is unconditional rather than an escalation the SIGTERM
    can make unnecessary: the leader exiting says nothing about a
    descendant that ignored SIGTERM, or one spawned in the window
    between the two signals. Killing an already-empty group is a
    suppressed `ProcessLookupError`, so paying for it always is free.
    """
    _signal_group(pgid, signal.SIGTERM)
    with contextlib.suppress(subprocess.TimeoutExpired):
        proc.wait(timeout=GROUP_TERM_GRACE)
    _signal_group(pgid, signal.SIGKILL)
    with contextlib.suppress(subprocess.TimeoutExpired):
        proc.wait(timeout=GROUP_KILL_GRACE)


def _log_tail(path: str, lines: int = 20) -> str:
    """The last `lines` of the preparation log, for the diagnostic.

    The full file stays on disk and is named beside this; the tail is
    what makes the failure legible without opening it.
    """
    try:
        with open(path, errors="replace") as handle:
            tail = [line.rstrip() for line in handle.read().splitlines()
                    if line.strip()][-lines:]
    except OSError:
        return ""
    return "\n".join(tail)


def _prepare_failure(message: str, log_path: str) -> "EnginePreparationError":
    """An `EnginePreparationError` carrying the build output.

    Requirement 3 of #1913 in one place: the operator gets the reason
    AND the output, rather than "engine never printed READY" pointing at
    an empty log the `-v0` fallback could never have written to.
    """
    tail = _log_tail(log_path)
    return EnginePreparationError(
        f"{message}; the build output is in {log_path}"
        + (f"\n--- {log_path} (tail) ---\n{tail}" if tail else ""))


def _run_prepare_step(argv, *, cwd, deadline, allowance, log_file, log_path,
                      capture: bool, what: str) -> str:
    """Run one preparation subprocess, owning its whole process group.

    `capture` selects where the child's stdout goes: the log file for
    the build (whose output is for the operator) and a pipe for `cabal
    list-bin` (whose stdout is the answer), with stderr reaching the log
    either way. Every exit that is not a clean zero — a nonzero status,
    the allowance running out, a `Ctrl-C`, an unlaunchable `cabal` —
    disposes of the group before it raises.
    """
    remaining = deadline - time.monotonic()
    if remaining <= 0:
        raise _prepare_failure(
            f"the engine executable could not be prepared: the allowance "
            f"ran out before {what} could start", log_path)
    try:
        proc = subprocess.Popen(
            argv, cwd=cwd,
            stdout=subprocess.PIPE if capture else log_file,
            stderr=log_file if capture else subprocess.STDOUT,
            text=True, start_new_session=True)
    except FileNotFoundError:
        raise EnginePreparationError(
            f"'cabal' was not found on PATH, so {ENGINE_TARGET} cannot be "
            f"prepared for this probe") from None
    except OSError as error:
        raise EnginePreparationError(
            f"could not run {' '.join(argv)}: {error}") from None
    # `start_new_session=True` makes the child a session AND process-group
    # leader, so its pgid IS its pid. It is captured HERE because once
    # `communicate` has reaped the leader, `os.getpgid(proc.pid)` raises —
    # leaving the descendants that are the whole point unaddressable.
    pgid = proc.pid
    try:
        out, _ = proc.communicate(timeout=remaining)
    except subprocess.TimeoutExpired:
        _reap_group(proc, pgid)
        with contextlib.suppress(Exception):
            proc.communicate(timeout=GROUP_KILL_GRACE)
        raise _prepare_failure(
            f"the engine executable could not be prepared: {what} did not "
            f"finish within the {allowance:g} s preparation allowance",
            log_path) from None
    except BaseException:
        # A Ctrl-C taken while the build was running, or anything else
        # unexpected: the group goes before the exception leaves.
        _reap_group(proc, pgid)
        raise
    if proc.returncode != 0:
        # The leader is already reaped; the group may not be. This is
        # the case a coordinated run actually observed.
        _reap_group(proc, pgid)
        raise _prepare_failure(
            f"the engine executable could not be prepared: "
            f"`{' '.join(argv)}` failed with exit status {proc.returncode}",
            log_path)
    return out or ""


@contextlib.contextmanager
def _build_state_hold(namespace, *, deadline, announce, lock_root):
    """Hold `cabal-build` EXCLUSIVELY for the duration of a preparation.

    Preparation is a Cabal WRITER, so it takes the same exclusive
    interest the aggregate runner's preflight takes and for the same
    reason: requirement 5 of #1913 is that the fix must not reintroduce
    #1570's defect one level down, with a hand-run probe's build landing
    inside a sweep's preflight or a `cabal repl` probe.

    The wait is BOUNDED by the preparation allowance rather than
    unbounded like `run_probes.preflight_hold`, because this one runs
    inside a probe: a probe that waits forever is indistinguishable from
    the hang this issue is about, and the caller needs a diagnostic
    naming the holder.
    """
    if not namespace:
        yield None
        return
    root = lock_root if lock_root is not None else PREPARE_LOCK_ROOT
    announced = False
    while True:
        try:
            hold = probe_resource_lock.acquire(
                exclusive={BUILD_RESOURCE}, namespace=namespace,
                root=root, purpose="probe engine preparation")
            break
        except probe_resource_lock.ResourceBusy as busy:
            if time.monotonic() >= deadline:
                raise EnginePreparationError(
                    f"the engine executable could not be prepared: "
                    f"{busy.describe()}; nothing was built, because "
                    f"building beside another writer is the defect this "
                    f"lock exists to prevent") from None
            if announce is not None and not announced:
                announce(f"waiting for {BUILD_RESOURCE!r}, held outside this "
                         f"probe, before preparing {ENGINE_TARGET} ...")
                announced = True
            time.sleep(max(0.0, min(PREPARE_LOCK_POLL,
                                    deadline - time.monotonic())))
        except probe_resource_lock.ResourceLockError as error:
            raise EnginePreparationError(
                f"the engine executable could not be prepared: the "
                f"{BUILD_RESOURCE!r} interest could not be established "
                f"({error})") from None
    try:
        yield hold
    finally:
        hold.release()


def default_prepare_log(tag) -> str:
    """Where preparation output goes when the caller names no path."""
    return os.path.join(tempfile.gettempdir(),
                        f"synarchy_probe_prepare_{tag}.log")


def prepare_executable(repo_root=None, *, environ=None, namespace=None,
                       timeout: float = DEFAULT_PREPARE_TIMEOUT,
                       log_path: str | None = None, announce=None,
                       lock_root=None) -> str:
    """The absolute engine executable, BUILT first if nobody supplied one.

    Aggregate mode is the early return: an executable handed over
    through `$SYNARCHY_PROBE_ENGINE_EXE` is validated and returned with
    no Cabal contact, no lock and no log file, so requirement 6 of #1913
    — a probe the runner launched prepares nothing — holds by
    construction rather than by the caller remembering to branch.

    Direct mode does what the runner's preflight does, in a probe: take
    `cabal-build` exclusively, run one freshness build and one `cabal
    list-bin`, release, and hand back the validated absolute path. The
    caller then execs that path, so the READY deadline it starts
    afterwards measures engine startup alone.

    `timeout` covers the WHOLE of it — waiting for the lock included —
    and is deliberately unrelated to any readiness allowance. Every
    failure is an `EnginePreparationError` naming preparation and the
    log the build output really went to.

    Nothing is memoized across calls, and that is not an oversight: a
    probe that boots twice makes exactly the two Cabal contacts per boot
    the `cabal run` fallback already made for it, so the count is
    unchanged and only their POSITION — outside the caller's clock —
    is new. On a warm tree the second is a freshness check that finds
    nothing to do.
    """
    inherited = runner_executable(environ)
    if inherited is not None:
        return inherited
    root = REPO_ROOT if repo_root is None else str(repo_root)
    if timeout <= 0:
        raise EnginePreparationError(
            f"the engine executable cannot be prepared with a "
            f"{timeout!r}-second allowance")
    path = (log_path if log_path
            else default_prepare_log(os.getpid()))
    try:
        token = (namespace if namespace is not None
                 else probe_resource_lock.repository_namespace(root))
    except probe_resource_lock.ResourceLockError as error:
        # Without a namespace there is no exclusion, and building
        # unlocked is the defect. This refuses rather than degrades.
        raise EnginePreparationError(
            f"the engine executable could not be prepared: the "
            f"{BUILD_RESOURCE!r} interest has no namespace to be taken in "
            f"({error})") from None
    if announce is not None:
        announce(f"preparing {ENGINE_TARGET} before the engine is launched; "
                 f"build output: {path}")
    deadline = time.monotonic() + timeout
    try:
        log_file = open(path, "w")
    except OSError as error:
        raise EnginePreparationError(
            f"the engine executable could not be prepared: its build log "
            f"{path} could not be opened ({error})") from None
    try:
        with _build_state_hold(token, deadline=deadline, announce=announce,
                               lock_root=lock_root):
            # One freshness build, then one read-only query: the same two
            # steps, in the same order, for the same reason as
            # `resolve_executable` — `cabal list-bin` answers with a path
            # whether or not that file is current.
            _run_prepare_step(
                ["cabal", "build", ENGINE_TARGET], cwd=root,
                deadline=deadline, allowance=timeout, log_file=log_file,
                log_path=path, capture=False,
                what=f"`cabal build {ENGINE_TARGET}`")
            located = _run_prepare_step(
                ["cabal", "list-bin", ENGINE_TARGET], cwd=root,
                deadline=deadline, allowance=timeout, log_file=log_file,
                log_path=path, capture=True,
                what=f"`cabal list-bin {ENGINE_TARGET}`")
    finally:
        log_file.close()
    try:
        return validate_executable(
            _last_line(located), source=f"`cabal list-bin {ENGINE_TARGET}`")
    except EngineExecutableError as error:
        raise _prepare_failure(
            f"the engine executable could not be prepared: {error}",
            path) from None


def prepare_command(args, *, repo_root=None, environ=None, **kwargs) -> list[str]:
    """`engine_command`, with the direct path's executable BUILT first.

    Under the runner this returns exactly what `engine_command` returns:
    the same absolute binary followed by the same engine arguments in
    the same order. Run by hand it returns that shape too rather than
    the `cabal run` fallback — the build has already happened, so there
    is nothing left for `cabal run` to do except re-check freshness
    inside the caller's clock.
    """
    return [prepare_executable(repo_root, environ=environ, **kwargs), *args]
