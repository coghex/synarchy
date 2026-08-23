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
  `cabal run` invocation probes have always used. Requirement 3 of #1570
  is that fallback, and it is the only place Cabal is still spelled.

The module deliberately imports nothing else from `tools/`:
`run_probes.py` imports it for the preflight, `probelib.py` imports it
for `boot`, and the four probes with their own private launchers import
it too — anything reaching back into those would close an import cycle.
"""
from __future__ import annotations

import os
import subprocess

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


class EngineExecutableError(RuntimeError):
    """The engine executable could not be resolved, or is unusable.

    Raised by the runner's preflight and by the validation of a
    runner-supplied path. Both are refusals BEFORE any probe starts, so
    the diagnostic has to name the repair rather than surface later as a
    probe assertion failure.
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
    repository root — and the caller falls back to `cabal run`. A value
    that IS present but unusable is an error rather than a silent
    fallback: falling back would put a `cabal run` back inside a parallel
    sweep, which is the whole defect.
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
