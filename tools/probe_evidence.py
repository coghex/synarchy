#!/usr/bin/env python3
"""Keep a failing probe's evidence by default (#2931).

Before this module a failing probe left only its printed `[FAIL]` lines:
most probes `mkdtemp` a scratch tree (engine log, save slot, fixtures) and
delete it in a `finally` whatever the outcome, `/tmp` is cleared by a
reboot, and on macOS `TMPDIR` is per session. Diagnosing one failure meant
repeating a run that can take an hour.

ONE retention rule, used by BOTH probe paths -- the legacy
`tools/run_probes.py` runner and the `probe-result/v1` harness
`tools/probe_flake.py`:

* the runner makes one ATTEMPT directory per probe execution under a
  durable root OUTSIDE every worktree, hands it to the child as
  `SYNARCHY_PROBE_EVIDENCE_DIR`, and points the child's `TMPDIR` into it,
  so every scratch tree the probe makes lives under it;
* a probe's own cleanup of a tree under that directory RETIRES it into
  the attempt instead of deleting it (`retire`, `discard_scratch`,
  `temporary_directory`, `release_tree`): the path is gone to the probe
  exactly as before, and the runner deletes the whole attempt on PASS and
  keeps it on FAIL, ERROR and TIMEOUT, printing its path;
* the engine logs a probe booted at fixed paths (`probelib.boot` lists
  them in `ENGINE_LOG_MANIFEST`) are copied into the kept attempt;
* a kept attempt carries a `.probe-evidence.json` marker, written LAST,
  and only marked attempts are ever pruned: the newest `DEFAULT_KEEP` per
  probe stay, an unmarked directory (an active or foreign one) is never
  touched, and a symlink is skipped rather than followed.

The durable root is `~/.local/state/synarchy/probe-failures` unless
`SYNARCHY_PROBE_FAILURE_ROOT` names another; `probe_flake --artifact-root`
overrides both. It must never resolve inside a worktree -- the primary
checkout has to stay clean for the PR drainer.

Nothing here changes a probe's checks or pass criteria, and a direct
`python3 tools/<name>_probe.py` run (no runner, so no
`SYNARCHY_PROBE_EVIDENCE_DIR`) behaves exactly as it always did.

Dependencies: the standard library only, so a probe can import it freely.
"""
from __future__ import annotations

import contextlib
import functools
import json
import os
import shutil
import subprocess
import sys
import tempfile
import uuid
from datetime import datetime, timezone
from pathlib import Path

# --- The runner -> probe environment ------------------------------------
# Where the operator wants failure evidence kept (the durable root).
ENV_ROOT = "SYNARCHY_PROBE_FAILURE_ROOT"
# How many finalized failed attempts to keep per probe.
ENV_KEEP = "SYNARCHY_PROBE_FAILURE_KEEP"
# Handed to a child by the runner: this attempt's evidence directory. Its
# presence is what tells a probe that the RUNNER decides what survives.
ENV_EVIDENCE_DIR = "SYNARCHY_PROBE_EVIDENCE_DIR"

DEFAULT_KEEP = 5
DEFAULT_ROOT_PARTS = (".local", "state", "synarchy", "probe-failures")

MARKER = ".probe-evidence.json"
MARKER_SCHEMA = "probe-evidence/v1"
# The child-side list of engine logs this attempt booted, one path a line.
ENGINE_LOG_MANIFEST = "engine-logs.txt"
# Where the runner copies those logs inside a kept attempt.
ENGINE_LOG_DIR = "engine-logs"
# Paths a probe owns OUTSIDE its attempt (the checkout's own saves/), listed
# before they are created so a killed probe's saves can still be archived.
OWNED_PATHS_MANIFEST = "owned-paths.txt"
OWNED_DIR = "owned-paths"
# One kept run's own completion time (see `stamp_run`).
RUN_STAMP = ".completed-at"
# The attempt's complete stdout/stderr capture, saved when it is kept.
CAPTURE_FILE = "stdout.txt"

# Outcomes. PASS is the only one that deletes.
PASS = "PASS"

REPO_ROOT = Path(__file__).resolve().parents[1]


class RootRejected(Exception):
    """The evidence root (or its configuration) cannot be used."""


# --------------------------------------------------------------------------
# The durable root
# --------------------------------------------------------------------------
def default_root() -> Path:
    return Path.home().joinpath(*DEFAULT_ROOT_PARTS)


def configured_root(environ=None) -> Path:
    """The durable root: `SYNARCHY_PROBE_FAILURE_ROOT`, else the default."""
    environ = os.environ if environ is None else environ
    value = environ.get(ENV_ROOT)
    return Path(value) if value else default_root()


def keep_count(environ=None) -> int:
    """How many finalized failures to keep per probe; a bad value is refused."""
    environ = os.environ if environ is None else environ
    raw = environ.get(ENV_KEEP)
    if raw is None or raw == "":
        return DEFAULT_KEEP
    try:
        count = int(raw)
    except ValueError:
        count = 0
    if count < 1:
        raise RootRejected(
            f"{ENV_KEEP}={raw!r} must be a positive count of failed attempts "
            f"to keep per probe")
    return count


def worktree_paths(repo_root: Path | None = None) -> list[Path]:
    """Every git worktree registered to this checkout, plus the checkout."""
    base = Path(repo_root or REPO_ROOT).resolve()
    paths = [base]
    try:
        done = subprocess.run(
            ["git", "worktree", "list", "--porcelain"], cwd=base, text=True,
            capture_output=True, timeout=30)
    except (OSError, subprocess.SubprocessError):
        return paths
    if done.returncode != 0:
        return paths
    for line in done.stdout.splitlines():
        if line.startswith("worktree "):
            with contextlib.suppress(OSError):
                paths.append(Path(line[len("worktree "):]).resolve())
    return paths


def check_root(root, trees=None) -> Path:
    """Resolve `root`, refusing anything inside a worktree or unusable.

    Raw probe artifacts must never land in a repository worktree: the
    primary checkout has to stay clean for the PR drainer, and a stray
    engine log inside an issue worktree wedges its post-merge cleanup.
    """
    try:
        resolved = Path(root).expanduser().resolve()
    except OSError as error:
        raise RootRejected(
            f"artifact root {root} cannot be resolved ({error})") from None
    for tree in (worktree_paths() if trees is None else trees):
        if resolved == tree or tree in resolved.parents:
            raise RootRejected(
                f"artifact root {resolved} is inside the working tree {tree}; "
                f"raw probe artifacts must never be written into a repository "
                f"worktree")
    try:
        resolved.mkdir(parents=True, exist_ok=True)
    except OSError as error:
        raise RootRejected(
            f"artifact root {resolved} cannot be created ({error})") from None
    if not os.access(resolved, os.W_OK | os.X_OK):
        raise RootRejected(f"artifact root {resolved} is not writable")
    return resolved


@functools.lru_cache(maxsize=1)
def _short_sha() -> str:
    try:
        done = subprocess.run(["git", "rev-parse", "--short", "HEAD"],
                              cwd=REPO_ROOT, text=True, capture_output=True,
                              timeout=30)
    except (OSError, subprocess.SubprocessError):
        return "nogit"
    return done.stdout.strip() or "nogit" if done.returncode == 0 else "nogit"


def _now() -> datetime:
    return datetime.now(timezone.utc)


# --------------------------------------------------------------------------
# Marker, pruning
# --------------------------------------------------------------------------
def _write_marker(directory: Path, probe: str, outcome: str,
                  kind: str = "attempt") -> None:
    """The marker is written LAST and atomically, so a half-finalized
    directory is never mistaken for a finished one and pruned under a
    reader."""
    document = {"schema": MARKER_SCHEMA, "probe": probe, "outcome": outcome,
                "kind": kind, "finalized_at": _now().strftime("%Y%m%dT%H%M%S.%fZ")}
    staging = directory / f"{MARKER}.{uuid.uuid4().hex[:8]}"
    staging.write_text(json.dumps(document) + "\n", encoding="utf-8")
    os.replace(staging, directory / MARKER)


def _read_marker(directory: Path) -> dict | None:
    try:
        document = json.loads((directory / MARKER).read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return None
    if isinstance(document, dict) and document.get("schema") == MARKER_SCHEMA:
        return document
    return None


def finalized_entries(probe_dir: Path) -> list[tuple[str, str, Path]]:
    """(finalized_at, name, path) of every finalized attempt, oldest first.

    Only a REAL directory carrying this module's marker counts. A symlink
    is skipped whatever it points at, so pruning can never reach outside
    the tree this module owns.
    """
    found = []
    try:
        children = sorted(probe_dir.iterdir())
    except OSError:
        return []
    for child in children:
        if child.is_symlink() or not child.is_dir():
            continue
        marker = _read_marker(child)
        if marker is None:
            continue
        found.append((str(marker.get("finalized_at", "")), child.name, child))
    return sorted(found)


def _failed_units(probe_dir: Path) -> list[tuple[str, str, str, Path, Path | None]]:
    """The failed ATTEMPTS under a probe's directory, oldest first.

    A legacy runner attempt is one unit. A `probe_flake` invocation holds a
    `run-NNN` directory per unsuccessful run and each of those is a unit of
    its own, so the retention count means failed attempts however the
    harness batched them. Each unit is `(finalized_at, directory name, run
    name, path, owning invocation or None)`.
    """
    units = []
    for stamp, name, path in finalized_entries(probe_dir):
        marker = _read_marker(path) or {}
        if marker.get("kind") != "invocation":
            units.append((stamp, name, "", path, None))
            continue
        try:
            runs = sorted(child for child in path.iterdir()
                          if child.name.startswith("run-")
                          and child.is_dir() and not child.is_symlink())
        except OSError:
            continue
        for run in runs:
            try:
                own = (run / RUN_STAMP).read_text(encoding="utf-8").strip()
            except OSError:
                own = ""
            units.append((own or stamp, name, run.name, run, path))
        if not runs:
            # Finalized and holding nothing: not evidence, only clutter.
            with contextlib.suppress(OSError):
                shutil.rmtree(path)
    return sorted(units, key=lambda unit: unit[:3])


def prune(probe_dir: Path, keep: int) -> list[str]:
    """Keep the newest `keep` failed attempts; return problems.

    Counting is per failed attempt (see `_failed_units`), so one harness
    invocation with many failing runs cannot hold the limit hostage. An
    invocation whose last run is pruned goes with it.
    """
    units = _failed_units(probe_dir)
    problems = []
    for _stamp, _name, _run, path, owner in units[:max(0, len(units) - keep)]:
        try:
            shutil.rmtree(path)
        except FileNotFoundError:
            pass  # a concurrent pruner got there first
        except OSError as error:
            problems.append(f"could not prune old failure evidence {path}: "
                            f"{error}")
            continue
        if owner is not None:
            try:
                left = [child for child in owner.iterdir()
                        if child.name.startswith("run-")]
            except OSError:
                continue
            if not left:
                with contextlib.suppress(OSError):
                    shutil.rmtree(owner)
    return problems


# --------------------------------------------------------------------------
# Engine logs booted outside the attempt
# --------------------------------------------------------------------------
def record_engine_log(path: str) -> None:
    """`probelib.boot` lists the log it opens, so the runner can keep it.

    A no-op outside a runner. One short `O_APPEND` write per boot, which
    concurrent writers cannot interleave.
    """
    evidence = os.environ.get(ENV_EVIDENCE_DIR)
    if not evidence:
        return
    with contextlib.suppress(OSError):
        manifest = os.path.join(evidence, ENGINE_LOG_MANIFEST)
        with open(manifest, "a", encoding="utf-8") as handle:
            handle.write(os.path.abspath(path) + "\n")


def archive_engine_logs(directory: Path) -> list[str]:
    """Copy every listed engine log into the kept attempt; return problems.

    A log already inside the attempt is left where it is. A copy that
    fails leaves its source alone and says where that source still is.
    """
    problems = []
    try:
        listed = (directory / ENGINE_LOG_MANIFEST).read_text(
            encoding="utf-8").splitlines()
    except OSError:
        return problems
    seen = set()
    target = directory / ENGINE_LOG_DIR
    inside = directory.resolve()
    for index, raw in enumerate(listed):
        if not raw or raw in seen:
            continue
        seen.add(raw)
        source = Path(raw)
        siblings = [source]
        siblings.append(source.with_name(source.name + ".prepare"))
        for candidate in siblings:
            if not candidate.is_file():
                continue
            with contextlib.suppress(OSError):
                if inside in candidate.resolve().parents:
                    continue
            try:
                target.mkdir(exist_ok=True)
                shutil.copyfile(candidate, target / f"{index:02d}-{candidate.name}")
            except OSError as error:
                problems.append(
                    f"could not copy engine log {candidate} into {directory}: "
                    f"{error}; the original is still at {candidate}")
    return problems


def register_owned_path(path) -> None:
    """List a path this probe will create OUTSIDE its attempt directory.

    Probes that write the checkout's own `saves/` cannot have it redirected
    by `TMPDIR`, and a probe killed by its timeout never reaches its own
    cleanup. Listing the path first lets the runner copy whatever is there
    into a kept attempt after the process group is gone. A no-op outside a
    runner; the original is never removed by the runner.
    """
    held = evidence_dir()
    if not held:
        return
    with contextlib.suppress(OSError):
        with open(os.path.join(held, OWNED_PATHS_MANIFEST), "a",
                  encoding="utf-8") as handle:
            handle.write(os.path.abspath(str(path)) + "\n")


def archive_owned_paths(directory: Path) -> list[str]:
    """Copy every registered path that still exists into the kept attempt.

    Returns problems; a copy that fails leaves the original alone and says
    where it still is. Paths already inside the attempt are skipped.
    """
    problems = []
    try:
        listed = (directory / OWNED_PATHS_MANIFEST).read_text(
            encoding="utf-8").splitlines()
    except OSError:
        return problems
    seen = set()
    inside = directory.resolve()
    for index, raw in enumerate(listed):
        if not raw or raw in seen:
            continue
        seen.add(raw)
        source = Path(raw)
        if not source.is_dir() or source.is_symlink():
            continue
        with contextlib.suppress(OSError):
            if inside in source.resolve().parents:
                continue
        target = directory / OWNED_DIR / f"{index:02d}-{source.name}"
        try:
            target.parent.mkdir(exist_ok=True)
            shutil.copytree(source, target, symlinks=True)
        except OSError as error:
            shutil.rmtree(target, ignore_errors=True)
            problems.append(
                f"could not copy {source} into {directory}: {error}; the "
                f"original is still at {source}")
    return problems


def stamp_run(run_dir: Path) -> None:
    """Record when THIS run completed, for pruning across invocations.

    An invocation is finalized once, at its end, so its runs would all
    inherit that moment; a run that failed earlier in a long batch would then
    outrank a failure from another invocation that finished in between.
    """
    with contextlib.suppress(OSError):
        (run_dir / RUN_STAMP).write_text(
            _now().strftime("%Y%m%dT%H%M%S.%fZ") + "\n", encoding="utf-8")


# --------------------------------------------------------------------------
# One attempt
# --------------------------------------------------------------------------
class Attempt:
    """One probe execution's evidence directory, from creation to verdict."""

    def __init__(self, retention: "Retention", probe: str, path: Path):
        self.retention = retention
        self.probe = probe
        self.path = path
        self.finished = False
        # Set when a PASS could not delete its directory AND the probe's
        # own cleanup was strict (`retire`): the run must then fail.
        self.removal_error: str | None = None

    @property
    def tmp(self) -> Path:
        return self.path / "tmp"

    def finish(self, outcome: str, *, announce=None,
               capture: str | None = None) -> Path | None:
        """Apply the retention rule; return the kept path, else None.

        PASS deletes the attempt (a deletion that fails is reported and the
        directory is returned as left behind -- never claimed removed).
        Anything else is kept, its engine logs are archived into it, it is
        marked finalized and the probe's older failures are pruned. Every
        problem goes to `announce` (default stderr) with the location of
        whatever remains.
        """
        say = announce or (lambda text: print(text, file=sys.stderr))
        if self.finished:
            return self.path if self.path.exists() else None
        self.finished = True
        if outcome == PASS:
            strict = (self.path / STRICT_MARK).exists()  # before the delete
            manifest = None
            if strict:
                # A delete that fails part-way has already taken files, the
                # engine-log list among them; keep it to rebuild below.
                with contextlib.suppress(OSError):
                    manifest = (self.path / ENGINE_LOG_MANIFEST).read_text(
                        encoding="utf-8")
            try:
                shutil.rmtree(self.path)
            except FileNotFoundError:
                return None
            except OSError as error:
                say(f"could not remove the passing run's scratch directory "
                    f"{self.path}: {error}")
                if not strict:
                    return self.path
                # A cleanup contract the runner could not finish: the run is
                # now a FAILURE, and goes through exactly the retention a
                # failure gets -- output, engine logs, marker, pruning.
                self.removal_error = (
                    f"the probe's retired scratch could not be removed "
                    f"({self.path}): {error}")
                outcome = "FAIL"
                if manifest is not None and not (
                        self.path / ENGINE_LOG_MANIFEST).exists():
                    with contextlib.suppress(OSError):
                        (self.path / ENGINE_LOG_MANIFEST).write_text(
                            manifest, encoding="utf-8")
                capture = (capture or "") + f"\n  [FAIL] {self.removal_error}\n"
            else:
                return None
        if capture is not None:
            try:
                (self.path / CAPTURE_FILE).write_text(
                    capture, encoding="utf-8", errors="replace")
            except OSError as error:
                say(f"could not save the captured output of {self.path}: "
                    f"{error}")
        for problem in archive_engine_logs(self.path):
            say(problem)
        for problem in archive_owned_paths(self.path):
            say(problem)
        try:
            _write_marker(self.path, self.probe, outcome)
        except OSError as error:
            say(f"could not mark failure evidence {self.path}: {error}; the "
                f"evidence itself is intact there")
            return self.path
        for problem in prune(self.path.parent, self.retention.keep):
            say(problem)
        return self.path


class Retention:
    """The resolved policy: where evidence goes and how much is kept."""

    def __init__(self, root: Path, keep: int, trees=None):
        self.root = root
        self.keep = keep
        # The worktrees the root and every per-probe directory must stay
        # out of; resolved once, with the root.
        self.trees = list(trees) if trees is not None else None

    @classmethod
    def from_environment(cls, environ=None, trees=None) -> "Retention":
        """Validate the durable root and keep count NOW, before any probe
        runs, so a misconfiguration is a refusal and never a lost failure."""
        environ = os.environ if environ is None else environ
        keep = keep_count(environ)
        trees = worktree_paths() if trees is None else trees
        return cls(check_root(configured_root(environ), trees), keep, trees)

    def probe_dir(self, probe: str) -> Path:
        return self.root / probe

    def check_probe_dir(self, probe: str) -> Path:
        """The per-probe directory, resolved and re-checked.

        `<root>/<probe>` may be a symlink an operator (or anyone) left
        there; following it must not carry evidence into a worktree or out
        of the root.
        """
        parent = self.probe_dir(probe)
        try:
            resolved = parent.resolve()
        except OSError as error:
            raise RootRejected(
                f"failure-evidence directory {parent} cannot be resolved "
                f"({error})") from None
        if self.trees is None:
            self.trees = worktree_paths()
        for tree in self.trees:
            if resolved == tree or tree in resolved.parents:
                raise RootRejected(
                    f"failure-evidence directory {parent} resolves to "
                    f"{resolved}, inside the working tree {tree}; raw probe "
                    f"artifacts must never be written into a repository "
                    f"worktree")
        if self.root != resolved and self.root not in resolved.parents:
            raise RootRejected(
                f"failure-evidence directory {parent} resolves to {resolved}, "
                f"outside the root {self.root}")
        return resolved

    def check_probes(self, probes) -> None:
        """Refuse BEFORE any probe runs if one's directory is unusable."""
        for probe in probes:
            self.check_probe_dir(probe)

    def begin(self, probe: str) -> Attempt:
        """A fresh, collision-free attempt directory for one execution:
        `<root>/<probe>/<UTC timestamp>-<short sha>-<nonce>`."""
        parent = self.check_probe_dir(probe)
        stamp = _now().strftime("%Y%m%dT%H%M%SZ")
        sha = _short_sha()
        while True:
            candidate = parent / f"{stamp}-{sha}-{uuid.uuid4().hex[:6]}"
            try:
                candidate.mkdir(parents=True)
            except FileExistsError:
                continue
            (candidate / "tmp").mkdir()
            return Attempt(self, probe, candidate)


def child_environment(evidence_dir) -> dict[str, str]:
    """What the runner hands a child so its scratch lands in `evidence_dir`."""
    tmp = os.path.join(str(evidence_dir), "tmp")
    os.makedirs(tmp, exist_ok=True)
    return {ENV_EVIDENCE_DIR: str(evidence_dir), "TMPDIR": tmp}


def finalize_invocation(invocation: Path, *, probe: str, retained: bool,
                        keep: int | None, announce=None) -> None:
    """Close a `probe_flake` invocation directory.

    The harness already deletes each passing run and retains each
    unsuccessful one. Here an invocation with nothing retained is removed
    outright; one with evidence is marked finalized (the caller has already
    archived each kept run's engine logs with `archive_engine_logs`) and,
    when `keep` is given (the durable root, not an operator-chosen one), its probe's older
    failures are pruned.
    """
    say = announce or (lambda text: print(text, file=sys.stderr))
    if not retained:
        with contextlib.suppress(OSError):
            invocation.rmdir()
        return
    try:
        _write_marker(invocation, probe, "FAIL", kind="invocation")
    except OSError as error:
        say(f"could not mark failure evidence {invocation}: {error}; the "
            f"evidence itself is intact there")
        return
    if keep is not None:
        for problem in prune(invocation.parent, keep):
            say(problem)


# --------------------------------------------------------------------------
# Probe side: the runner decides what survives
# --------------------------------------------------------------------------
def evidence_dir(environ=None) -> str | None:
    environ = os.environ if environ is None else environ
    return environ.get(ENV_EVIDENCE_DIR) or None


def is_runner_owned(path, environ=None) -> bool:
    """True when `path` lies inside the evidence directory a runner gave us.

    A path outside it (a probe that kept its tree elsewhere, a stale export)
    is the probe's own to clean, exactly as before.
    """
    held = evidence_dir(environ)
    if not held:
        return False
    try:
        inside = os.path.realpath(held)
        target = os.path.realpath(str(path))
    except OSError:
        return False
    return target == inside or target.startswith(inside + os.sep)


RETIRED_DIR = "retired"


STRICT_MARK = ".strict-cleanup"


def _retire(path, *, strict: bool) -> None:
    if not is_runner_owned(path):
        shutil.rmtree(path)
        return
    held = os.path.realpath(evidence_dir())
    if os.path.realpath(str(path)) == held:
        raise OSError(f"refusing to retire the evidence directory {path} itself")
    retired = os.path.join(held, RETIRED_DIR)
    os.makedirs(retired, exist_ok=True)
    name = f"{os.path.basename(str(path).rstrip(os.sep)) or 'tree'}-{uuid.uuid4().hex[:6]}"
    os.rename(path, os.path.join(retired, name))
    if strict:
        # This probe's cleanup is a contract: a tree it could not remove is
        # a failed run. The removal now happens in the runner, so say so
        # there (`Attempt.finish`).
        with contextlib.suppress(OSError):
            Path(held, STRICT_MARK).touch()


def retire(path) -> None:
    """Take a probe-owned tree out of the way; raises `OSError` on failure.

    Run directly this is `shutil.rmtree(path)`. Under a runner, a tree
    inside the attempt directory is RENAMED into the attempt's `retired/`
    directory instead: to the probe the path is gone exactly as if it had
    been deleted -- so a "did the removal work" check and a later reuse of
    the name behave the same -- while the contents stay for the runner,
    which deletes the whole attempt on PASS and keeps it on any other
    outcome. A probe cannot know its own outcome at cleanup time, which is
    why the decision is not made here.

    This is the STRICT form, for cleanup whose failure the probe reports:
    the attempt is marked so that, if the runner's own deletion of the
    retired tree later fails, a passing run is turned into a failure
    rather than leaving a green result beside leftover artifacts.
    `discard_scratch` is the lenient form.
    """
    _retire(path, strict=True)


def discard_scratch(path) -> None:
    """A probe's `finally`-time removal of its own scratch tree, errors
    ignored: `shutil.rmtree(path, ignore_errors=True)` when run directly,
    a lenient `_retire` under a runner."""
    try:
        _retire(path, strict=False)
    except OSError:
        pass


def retire_saves(path, *, strict: bool = False) -> None:
    """Remove a probe-owned save slot that lives OUTSIDE the attempt.

    Probes that write the checkout's own `saves/` cannot have it redirected
    by `TMPDIR`. Run directly this removes the slot as the probe always
    did. Under a runner the slot is first copied into the attempt's
    `retired/` directory, then removed, so a failing run keeps the save it
    needs for diagnosis while the shared `saves/` still ends up clean. A
    copy that fails leaves the original where it is and says so -- losing
    the only copy of the evidence is the worse outcome. `strict`
    mirrors `shutil.rmtree(path)` (errors raise) instead of
    `ignore_errors=True`.
    """
    held = evidence_dir()
    if held and not is_runner_owned(path):
        register_owned_path(path)
    if held and os.path.isdir(path) and not is_runner_owned(path):
        destination = os.path.join(
            held, RETIRED_DIR,
            f"{os.path.basename(str(path).rstrip(os.sep))}-{uuid.uuid4().hex[:6]}")
        try:
            os.makedirs(os.path.dirname(destination), exist_ok=True)
            shutil.copytree(path, destination, symlinks=True)
        except OSError as error:
            # No copy exists, so the original IS the evidence: it stays
            # where it is, and the failure names it. A strict caller fails
            # (its cleanup did not happen); a lenient one is told on stderr.
            shutil.rmtree(destination, ignore_errors=True)
            message = (f"could not keep a copy of {path} as failure evidence "
                       f"({error}); the original is left in place at {path}")
            if strict:
                raise OSError(message) from error
            print(message, file=sys.stderr)
            return
    if strict:
        shutil.rmtree(path)
    else:
        shutil.rmtree(path, ignore_errors=True)


@contextlib.contextmanager
def temporary_directory(prefix: str = "tmp"):
    """`tempfile.TemporaryDirectory(prefix=...)` that retires under a runner.

    Under a runner `TMPDIR` already points into the attempt directory, so
    the tree is created there. Like `TemporaryDirectory`, a cleanup that
    FAILS raises (a vanished tree does not): a probe that left its scratch
    behind must not exit successfully.
    """
    path = tempfile.mkdtemp(prefix=prefix)
    try:
        yield path
    finally:
        try:
            retire(path)
        except FileNotFoundError:
            pass


REMOVAL_FAILED = "could not remove this run's artifact directory {base}: {error}"
SURVIVED = "this run's artifact directory survived removal: {base}"


def release_tree(base: str, *, keep: bool = False, describe=None,
                 removal_failed: str = REMOVAL_FAILED,
                 survived: str = SURVIVED) -> str | None:
    """Retire one invocation-owned tree; return a failure message, else None.

    The shared core of the `release_artifacts` the flora and location
    probes used to carry as copies.

    * `keep` (`--keep-artifacts`): retained where it is and named, on PASS
      too.
    * otherwise the tree is `retire`d, and anything that SURVIVES is a
      reported failure: a green result sitting beside leftover artifacts is
      precisely what these probes' isolation exists to prevent. Under a
      runner the retired tree is the failure evidence, and its path is
      the runner's to print.

    `describe` is called with a `print`-like callable to list what the tree
    holds, so each probe keeps naming the parts it actually produced, and
    `removal_failed` / `survived` are the probe's own wording for the two
    ways a removal can fail (`{base}` and `{error}` are filled in).
    """
    if keep:
        print(f"\nretained this run's artifacts (--keep-artifacts): {base}")
        if describe is not None:
            describe(print)
        return None
    try:
        retire(base)
    except OSError as error:
        return removal_failed.format(base=base, error=error)
    if os.path.exists(base):
        return survived.format(base=base, error="")
    return None
