#!/usr/bin/env python3
"""Repeat-run flakiness measurement for one protocol-compatible probe (#1425).

Nine manual-only probes are classified `flaky` and several more have
known flakes recorded under other reasons, but none of that is measured:
"flaky" is a comment someone wrote after a bad afternoon. This runs ONE
registered probe N times in a row under a fixed, realistic RTS setting
and reports a per-check PASS/FAIL/MISSING table plus an aggregate
failure rate that a later census (#1426/#1428) can act on.

    python3 tools/probe_flake.py --probe role --runs 10
    python3 tools/probe_flake.py --probe role --runs 10 --result /tmp/role.json

Only probes that implement the shared `probe-result/v1` protocol
(`tools/probe_protocol.py`) can be measured. Everything else is
`legacy` and is REJECTED before execution, by name, without running the
probe at all — heuristically parsing free-form stdout is exactly the
guesswork a reliability harness must not do, and invoking a legacy probe
to find out would boot a real engine (~11 minutes for `farm_ai`).

What this owns, and what it deliberately does not:

* `run_probes.run_one` still owns process launch, combined output
  capture, elapsed timing, deferred SIGINT, timeout escalation, and
  process-group cleanup. This module reuses it rather than growing a
  second subprocess lifecycle.
* Check identity comes from the PROBE's own descriptor, never from the
  census manifest (`tools/probe_census.py`), which lives outside the
  repository by design. Eligibility comes from `PROTOCOL_PROBES` below,
  for the same reason: the harness must behave identically whether or
  not a `docs-wip` worktree exists.

Exit codes:
  0  a valid measurement, WHATEVER rate it observed — measuring
     flakiness is the point, so a 100% failure rate still exits 0
  2  rejected before execution (unknown probe, CI-eligible probe,
     legacy probe, descriptor mismatch, bad run/capability count,
     port 8008, unusable artifact root)
  3  no clear, leasable port in the whole range
  4  harness error: the protocol stream was malformed, truncated,
     duplicated, out of order, or otherwise unclassifiable, so no
     trustworthy rate exists
"""
from __future__ import annotations

import argparse
import contextlib
import fcntl
import json
import os
import shutil
import socket
import subprocess
import sys
import tempfile
import time
import uuid
from datetime import datetime, timezone
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ci_probes  # noqa: E402
import probe_protocol  # noqa: E402
import run_probes  # noqa: E402

RESULT_SCHEMA = "probe-flake-result/v1"

# Probes that implement `probe-result/v1`. This is the IN-REPO source of
# truth for eligibility — `tools/probe_census.py` reads it to fill the
# manifest's protocol-status column, never the other way round, so the
# harness works on a checkout with no docs worktree. Later migration
# issues add one key each.
PROTOCOL_PROBES: dict[str, str] = {
    "role": probe_protocol.PROTOCOL_VERSION,
}

# 8008 is the user's graphical instance. It is outside the range below
# and is refused unconditionally anyway, because "the range never
# contains it" is an invariant a future edit could quietly break.
FORBIDDEN_PORT = 8008
PORT_MIN = 8009
PORT_MAX = 8999

# A deliberate, realistic test condition (#1425): consistent across runs
# and machines, and enough capabilities to cover blocking/threading
# behavior. NOT a throughput setting, and there is no automatic fallback
# — a setup or budget failure at this level is a result to report, not a
# condition to tune away.
DEFAULT_RTS_CAPS = 4

DEFAULT_TIMEOUT = run_probes.DEFAULT_TIMEOUT

ARTIFACT_DIR_NAME = "synarchy-probe-flake"
LEASE_DIR_NAME = "synarchy-probe-flake-leases"

def _machine_wide_scratch() -> Path:
    """The fixed per-user location the port leases and registry live in.

    Deliberately NOT under the artifact root, and deliberately NOT
    `tempfile.gettempdir()`. Both vary per invocation — the artifact
    root because `--artifact-root` overrides it, the temp dir because it
    follows `TMPDIR`, which on macOS is a PER-SESSION `/var/folders/...`
    path and on any platform is whatever the launching shell exported.
    Either one splitting the namespace means two harnesses lock
    different files, both believe they own a port, and put two engines
    on it — the exact collision atomic leasing exists to prevent — while
    their concurrency registries also stop seeing each other.

    `/tmp` is the fixed, shared path POSIX guarantees; the uid suffix
    keeps one user's namespace out of another's, and `_ensure_owned_dir`
    refuses to share a directory this user does not own. Resolved once
    at import; only the self-test may redirect it, by rebinding
    `LEASE_ROOT` explicitly.
    """
    return Path("/tmp") / f"{LEASE_DIR_NAME}-{os.getuid()}"


LEASE_ROOT = _machine_wide_scratch()

# How long a live-invocation registration whose owner is gone is left
# alone before it is treated as stale. Short, because the owning process
# is already proven dead; nonzero so a registration a starting harness
# is still publishing is never mistaken for garbage. Port leases do NOT
# use this: an `flock` needs no staleness heuristic (see `PortLease`).
STALE_AFTER = 30.0

EXIT_OK = 0
EXIT_REJECTED = 2
EXIT_NO_PORT = 3
EXIT_HARNESS_ERROR = 4

RUN_PASS = "PASS"
RUN_FAIL = "FAIL"
RUN_TIMEOUT = "TIMEOUT"
# Not a probe outcome. Only ever the label on `Measurement.error_run`,
# the one run whose protocol stream could not be trusted.
RUN_HARNESS_ERROR = "HARNESS_ERROR"


class Rejection(Exception):
    """Refused before any probe ran."""


class PortExhausted(Exception):
    """A complete wrapped scan found no clear, leasable port."""


class HarnessError(Exception):
    """The protocol stream could not be trusted; no rate is reportable."""

    def __init__(self, message: str, run_index: int | None = None):
        super().__init__(message)
        self.run_index = run_index


# --------------------------------------------------------------------------
# Registry / eligibility
# --------------------------------------------------------------------------
def registered_scripts() -> dict[str, str]:
    """Every registered probe key -> its script filename."""
    return {key: script for key, script, _purpose in run_probes.PROBES}


def protocol_status(key: str) -> str:
    """`probe-result/v1` for a migrated probe, `legacy` for everything else."""
    return PROTOCOL_PROBES.get(key, "legacy")


def resolve_probe(key: str) -> str:
    """The script filename for `key`, or a `Rejection` naming the reason.

    Every rejection here happens BEFORE any subprocess exists. That is
    the load-bearing part for a legacy probe: running one to discover it
    is legacy would boot a real engine.
    """
    scripts = registered_scripts()
    if key not in scripts:
        raise Rejection(
            f"unknown probe {key!r}: not registered in run_probes.PROBES. "
            f"`python3 tools/run_probes.py --list` names every probe.")
    if key in ci_probes.CI_ELIGIBLE:
        raise Rejection(
            f"probe {key!r} is CI-eligible, and the flakiness census "
            f"(#1426) covers manual-only probes. CI already runs it on "
            f"every matching PR.")
    status = protocol_status(key)
    if status != probe_protocol.PROTOCOL_VERSION:
        raise Rejection(
            f"probe {key!r} is {status} and requires migration to "
            f"{probe_protocol.PROTOCOL_VERSION} before it can be measured. "
            f"This harness does not interpret a probe's stdout.")
    return scripts[key]


def fetch_descriptor(key: str, script: str,
                     timeout: float = 60.0) -> probe_protocol.Descriptor:
    """Ask a migrated probe to declare its checks; boots no engine.

    `--describe` is required to be a pure print-and-exit path, so this
    is safe to run before the measurement starts and is where a
    descriptor naming the wrong probe or an unsupported schema version
    is caught.
    """
    cmd = ["python3", os.path.join("tools", script), "--describe"]
    try:
        done = subprocess.run(cmd, cwd=run_probes.REPO_ROOT, text=True,
                              capture_output=True, timeout=timeout)
    except (OSError, subprocess.SubprocessError) as error:
        raise Rejection(
            f"probe {key!r}: could not obtain its {probe_protocol.PROTOCOL_VERSION} "
            f"descriptor ({error})") from None
    if done.returncode != 0:
        raise Rejection(
            f"probe {key!r}: `--describe` exited {done.returncode}; it does "
            f"not implement {probe_protocol.PROTOCOL_VERSION}. "
            f"{(done.stderr or done.stdout or '').strip()[:300]}")
    try:
        return probe_protocol.parse_descriptor(done.stdout, expected_probe=key)
    except probe_protocol.ProtocolError as error:
        raise Rejection(f"probe {key!r}: {error}") from None


# --------------------------------------------------------------------------
# Ports: atomic cross-process leasing
# --------------------------------------------------------------------------
def _pid_alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    except OSError:
        return True
    return True


def _ensure_owned_dir(path: Path) -> Path:
    """Create `path` privately, refusing one another user owns.

    The lease namespace is under a shared `/tmp`, so "it exists" is not
    the same as "it is mine": writing leases into a directory another
    account created would make this harness's exclusion meaningless
    against its own concurrent runs.
    """
    try:
        path.mkdir(parents=True, exist_ok=True, mode=0o700)
        owner = path.stat().st_uid
    except OSError as error:
        raise Rejection(
            f"could not create the harness scratch directory {path} "
            f"({error})") from None
    if owner != os.getuid():
        raise Rejection(
            f"the harness scratch directory {path} belongs to uid {owner}, "
            f"not {os.getuid()}; refusing to share a port-lease namespace "
            f"with another user")
    return path


def _lease_dir() -> Path:
    _ensure_owned_dir(LEASE_ROOT)
    return _ensure_owned_dir(LEASE_ROOT / "ports")


def port_is_clear(port: int, host: str = "127.0.0.1") -> bool:
    """True when nothing is listening on `port` right now."""
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.settimeout(0.5)
        return sock.connect_ex((host, port)) != 0


class PortLease:
    """An advisory `flock` on a per-port file: the holder owns the port.

    The lock, NOT the file, is the lease. That distinction is the whole
    design: a create-then-delete protocol has to decide when an
    abandoned lease has gone stale, and that decision is unavoidably
    racy — two harnesses can both judge one lease stale, and the second
    then deletes the FRESH lease the first just made, putting two
    engines on one port. `flock(LOCK_EX | LOCK_NB)` has no such window.
    Only one open file description can hold it at a time, across
    processes and within one, and the kernel drops it when its owner
    exits however abruptly. Stale recovery is therefore not implemented
    here at all: there is nothing to recover, because a dead harness
    holds nothing. The lease FILE is never unlinked — it is a few bytes
    of diagnostics naming the current holder, and unlinking it is
    exactly the operation that would reintroduce the race.
    """

    def __init__(self, port: int, path: Path, fd: int):
        self.port = port
        self.path = path
        self._fd = fd

    @classmethod
    def try_acquire(cls, port: int) -> "PortLease | None":
        if port == FORBIDDEN_PORT:
            # Unconditional, not merely "outside the range": this is the
            # one guard that survives an edit to PORT_MIN/PORT_MAX.
            raise Rejection(
                f"port {FORBIDDEN_PORT} is the user's graphical instance and "
                f"is always forbidden")
        path = _lease_dir() / f"{port}.lease"
        fd = os.open(path, os.O_CREAT | os.O_RDWR, 0o644)
        try:
            fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except OSError:
            # Held by someone else — including another lease object in
            # this same process, since flock conflicts between open file
            # descriptions rather than between processes.
            os.close(fd)
            return None
        except BaseException:
            os.close(fd)
            raise
        try:
            os.ftruncate(fd, 0)
            os.write(fd, json.dumps(
                {"pid": os.getpid(), "port": port, "acquired": time.time()}
            ).encode("utf-8"))
        except OSError:
            # Diagnostics only; the lock is what matters and we hold it.
            pass
        return cls(port, path, fd)

    def release(self) -> None:
        with contextlib.suppress(OSError):
            fcntl.flock(self._fd, fcntl.LOCK_UN)
        with contextlib.suppress(OSError):
            os.close(self._fd)


def acquire_port(cursor: int) -> tuple[PortLease, int]:
    """Lease the first clear port at or after `cursor`, wrapping once.

    Returns `(lease, next_cursor)`. Raises `PortExhausted` after one
    complete scan of the range, without ever having started a probe —
    the alternative is launching an engine onto a port another harness's
    engine already holds.
    """
    span = PORT_MAX - PORT_MIN + 1
    start = PORT_MIN + ((cursor - PORT_MIN) % span)
    for step in range(span):
        port = PORT_MIN + ((start - PORT_MIN + step) % span)
        if port == FORBIDDEN_PORT:
            continue
        lease = PortLease.try_acquire(port)
        if lease is None:
            continue
        if not port_is_clear(port):
            # Leased but occupied: something outside this harness holds
            # it. Give the lease back rather than sitting on it.
            lease.release()
            continue
        return lease, PORT_MIN + ((port - PORT_MIN + 1) % span)
    raise PortExhausted(
        f"no clear, leasable port in {PORT_MIN}-{PORT_MAX} after a complete "
        f"scan; nothing was started")


# --------------------------------------------------------------------------
# Live-invocation registry (the concurrency figure)
# --------------------------------------------------------------------------
class LiveRegistry:
    """Registers this invocation machine-wide and samples peak concurrency.

    "Concurrency" is the PEAK number of live harness invocations
    observed during this one, including itself, so a solo run always
    reports 1. Sampling is deterministic: once on entry, once before
    every run, and once after every run.
    """

    def __init__(self):
        self.path = (LEASE_ROOT / "live" /
                     f"{os.getpid()}-{uuid.uuid4().hex[:8]}.json")
        self.peak = 0
        self._registered = False

    def __enter__(self) -> "LiveRegistry":
        _ensure_owned_dir(LEASE_ROOT)
        _ensure_owned_dir(self.path.parent)
        # PUBLISHED ATOMICALLY. Writing straight to the final path would
        # leave a window in which a concurrently starting harness reads
        # an empty file, classifies it as corrupt, and unlinks it — and
        # this harness then vanishes from every later concurrency
        # sample while still running. `os.replace` within the same
        # directory means the final name either does not exist or holds
        # complete content, and the staging name never ends in `.json`
        # so a concurrent scan skips it.
        staging = self.path.with_suffix(".staging")
        staging.write_text(
            json.dumps({"pid": os.getpid(), "started": time.time()}),
            encoding="utf-8")
        os.replace(staging, self.path)
        self._registered = True
        self.sample()
        return self

    def __exit__(self, *exc) -> None:
        if self._registered:
            with contextlib.suppress(OSError):
                self.path.unlink()
            self._registered = False
        return None

    def live_count(self) -> int:
        """Live registrations, reaping any whose owning process is gone."""
        directory = self.path.parent
        count = 0
        try:
            entries = list(directory.iterdir())
        except OSError:
            return max(self.peak, 1)
        for entry in entries:
            if entry.suffix != ".json":
                continue
            try:
                age = time.time() - entry.stat().st_mtime
            except OSError:
                # It went away underneath us — its owner departed.
                continue
            try:
                pid = int(json.loads(entry.read_text(encoding="utf-8"))["pid"])
            except (OSError, KeyError, TypeError, ValueError):
                # Corrupt. Old enough to be nobody's, so reap it;
                # otherwise leave it alone and COUNT it. Nothing young
                # is ever deleted here — over-counting concurrency by
                # one is harmless, while deleting a registration a live
                # harness owns loses it permanently.
                if age >= STALE_AFTER:
                    with contextlib.suppress(OSError):
                        entry.unlink()
                    continue
                count += 1
                continue
            if not _pid_alive(pid) and age >= STALE_AFTER:
                with contextlib.suppress(OSError):
                    entry.unlink()
                continue
            count += 1
        return max(count, 1)

    def sample(self) -> int:
        current = self.live_count()
        self.peak = max(self.peak, current)
        return current


# --------------------------------------------------------------------------
# Artifacts
# --------------------------------------------------------------------------
def default_artifact_root() -> Path:
    """`<platform temp dir>/synarchy-probe-flake`.

    Resolved through `tempfile.gettempdir()` rather than a bare
    `${TMPDIR}`, which expands to nothing — and so to an unwritable
    `/synarchy-probe-flake` — under the many shells that do not set it.
    """
    return Path(tempfile.gettempdir()) / ARTIFACT_DIR_NAME


def _worktree_paths() -> list[Path]:
    """Every git worktree registered to this checkout, plus the checkout."""
    paths = [Path(run_probes.REPO_ROOT).resolve()]
    try:
        done = subprocess.run(
            ["git", "worktree", "list", "--porcelain"],
            cwd=run_probes.REPO_ROOT, text=True, capture_output=True, timeout=30)
    except (OSError, subprocess.SubprocessError):
        return paths
    if done.returncode != 0:
        return paths
    for line in done.stdout.splitlines():
        if line.startswith("worktree "):
            with contextlib.suppress(OSError):
                paths.append(Path(line[len("worktree "):]).resolve())
    return paths


def check_artifact_root(root: Path) -> Path:
    """Resolve `root`, refusing anything inside a working tree or unusable.

    Raw probe artifacts must never land in a repository worktree: the
    primary checkout has to stay clean for the PR drainer, and a stray
    engine log inside an issue worktree wedges its post-merge cleanup.

    An unusable root is a pre-execution REJECTION like any other, not a
    traceback: `--artifact-root /dev/null/x` must exit with the
    documented rejection code naming the path, before a probe or a port
    lease exists.
    """
    try:
        resolved = Path(root).expanduser().resolve()
    except OSError as error:
        raise Rejection(
            f"artifact root {root} cannot be resolved ({error})") from None
    for tree in _worktree_paths():
        if resolved == tree or tree in resolved.parents:
            raise Rejection(
                f"artifact root {resolved} is inside the working tree {tree}; "
                f"raw probe artifacts must never be written into a repository "
                f"worktree")
    try:
        resolved.mkdir(parents=True, exist_ok=True)
    except OSError as error:
        raise Rejection(
            f"artifact root {resolved} cannot be created ({error})") from None
    if not os.access(resolved, os.W_OK | os.X_OK):
        raise Rejection(f"artifact root {resolved} is not writable")
    return resolved


def new_invocation_dir(root: Path, probe: str) -> Path:
    """A collision-free directory for this invocation, created fresh."""
    stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    while True:
        candidate = root / f"{probe}-{stamp}-{os.getpid()}-{uuid.uuid4().hex[:8]}"
        try:
            candidate.mkdir(parents=True)
        except FileExistsError:
            continue
        except OSError as error:
            raise Rejection(
                f"could not create an artifact directory under {root} "
                f"({error})") from None
        return candidate


def _remove_tree(path: Path) -> None:
    with contextlib.suppress(OSError):
        shutil.rmtree(path)


# --------------------------------------------------------------------------
# Measurement
# --------------------------------------------------------------------------
class RunRecord:
    def __init__(self, index: int, port: int, outcome: str, elapsed: float,
                 checks: dict[str, str], artifact_dir: Path | None):
        self.index = index
        self.port = port
        self.outcome = outcome
        self.elapsed = elapsed
        self.checks = checks
        self.artifact_dir = artifact_dir

    def to_document(self) -> dict:
        return {
            "index": self.index,
            "port": self.port,
            "outcome": self.outcome,
            "elapsed_seconds": round(self.elapsed, 3),
            "checks": dict(self.checks),
            "artifact_dir": str(self.artifact_dir) if self.artifact_dir else None,
        }


class Measurement:
    def __init__(self, probe: str, descriptor: probe_protocol.Descriptor,
                 requested_runs: int, rts_caps: int, artifact_root: Path,
                 invocation_dir: Path):
        self.probe = probe
        self.descriptor = descriptor
        self.requested_runs = requested_runs
        self.rts_caps = rts_caps
        self.artifact_root = artifact_root
        self.invocation_dir = invocation_dir
        self.runs: list[RunRecord] = []
        # The run whose protocol stream could not be trusted, if any.
        # Deliberately NOT in `runs`: that list is the complete VALID
        # per-run outcome list, and a harness error is not a fourth
        # probe outcome. It still has to be reported, with its retained
        # artifacts and whatever partial checks parsed cleanly, or the
        # measurement would claim nothing went wrong.
        self.error_run: RunRecord | None = None
        self.peak_concurrency = 1
        self.status = "ok"
        self.error: str | None = None
        self.commit_sha = _commit_sha()
        self.timestamp = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

    # -- derived -----------------------------------------------------------
    @property
    def valid(self) -> bool:
        return self.status == "ok"

    @property
    def timeout_count(self) -> int:
        return sum(1 for r in self.runs if r.outcome == RUN_TIMEOUT)

    @property
    def failure_count(self) -> int:
        return sum(1 for r in self.runs
                   if r.outcome in (RUN_FAIL, RUN_TIMEOUT))

    @property
    def failure_rate(self) -> float | None:
        """Failures + timeouts over REQUESTED runs, or None if untrustworthy."""
        if not self.valid or not self.requested_runs:
            return None
        return self.failure_count / self.requested_runs

    @property
    def worst_elapsed(self) -> float:
        return max((r.elapsed for r in self.runs), default=0.0)

    @property
    def total_elapsed(self) -> float:
        return sum(r.elapsed for r in self.runs)

    def check_counts(self) -> dict[str, dict[str, int]]:
        counts = {cid: {probe_protocol.PASS: 0, probe_protocol.FAIL: 0,
                        probe_protocol.MISSING: 0}
                  for cid in self.descriptor.ids}
        for record in self.runs:
            for cid, result in record.checks.items():
                counts[cid][result] += 1
        return counts

    def retained_artifacts(self) -> list[str]:
        records = list(self.runs)
        if self.error_run is not None:
            records.append(self.error_run)
        return [str(r.artifact_dir) for r in records if r.artifact_dir]

    # -- serialization -----------------------------------------------------
    def to_document(self) -> dict:
        rate = self.failure_rate
        return {
            "schema": RESULT_SCHEMA,
            "probe": self.probe,
            "status": self.status,
            "error": self.error,
            "requested_runs": self.requested_runs,
            "completed_runs": len(self.runs),
            "runs": [r.to_document() for r in self.runs],
            "error_run": (self.error_run.to_document()
                          if self.error_run is not None else None),
            "checks": [{"id": cid, "label": label}
                       for cid, label in self.descriptor.checks],
            "check_counts": self.check_counts(),
            "failure_count": self.failure_count,
            "failure_rate": None if rate is None else round(rate, 6),
            "timeout_count": self.timeout_count,
            "worst_elapsed_seconds": round(self.worst_elapsed, 3),
            "total_elapsed_seconds": round(self.total_elapsed, 3),
            "timestamp_utc": self.timestamp,
            "commit_sha": self.commit_sha,
            "rts_capabilities": self.rts_caps,
            "peak_concurrency": self.peak_concurrency,
            "artifact_root": str(self.artifact_root),
            "invocation_dir": str(self.invocation_dir),
            "retained_artifacts": self.retained_artifacts(),
        }


def _commit_sha() -> str:
    try:
        done = subprocess.run(["git", "rev-parse", "HEAD"],
                              cwd=run_probes.REPO_ROOT, text=True,
                              capture_output=True, timeout=30)
    except (OSError, subprocess.SubprocessError):
        return "unknown"
    return done.stdout.strip() if done.returncode == 0 else "unknown"


def salvage_checks(descriptor: probe_protocol.Descriptor,
                   events_text: str) -> dict[str, str]:
    """The valid partial data a broken run's event stream still holds.

    Everything up to the first bad line is TRUSTED — it parsed under the
    same rules a clean stream does — so an `alpha` PASS followed by a
    malformed line still reports that PASS. Only what follows the fault
    is discarded, and every check that never arrived stays MISSING.
    """
    _events, outcomes, _error = probe_protocol.scan_event_stream(
        events_text, descriptor)
    return outcomes


def reconcile(descriptor: probe_protocol.Descriptor, ok: bool, timed_out: bool,
              events_text: str, stdout_text: str) -> tuple[str, dict[str, str]]:
    """One run's outcome and per-check results.

    TIMEOUT wins outright, whatever partial checks arrived. Otherwise a
    FAIL check or a nonzero exit makes the run FAIL, and only a zero
    exit with no failed check makes it PASS. A malformed stream or a
    bracketed stdout marker is neither — it raises, because an
    unreadable result must never be reported as a probe PASS.
    """
    markers = probe_protocol.forbidden_marker_lines(stdout_text)
    if markers:
        raise HarnessError(
            "the probe printed bracketed stdout markers while in protocol "
            "mode, which would be a second result channel beside the event "
            f"stream: {markers[:3]}")
    try:
        _events, outcomes = probe_protocol.parse_event_stream(
            events_text, descriptor)
    except probe_protocol.ProtocolError as error:
        raise HarnessError(str(error)) from None
    if timed_out:
        return RUN_TIMEOUT, outcomes
    failed = any(v == probe_protocol.FAIL for v in outcomes.values())
    if failed or not ok:
        return RUN_FAIL, outcomes
    return RUN_PASS, outcomes


def measure(probe: str, runs: int, *, artifact_root: Path | None = None,
            rts_caps: int = DEFAULT_RTS_CAPS, timeout: float = DEFAULT_TIMEOUT,
            start_port: int = PORT_MIN, announce=None) -> Measurement:
    """Run `probe` `runs` times sequentially and reconcile every run.

    Raises `Rejection` before anything is started, `PortExhausted` when
    the range is unusable, and returns a Measurement whose `status` is
    `harness-error` when a run's protocol stream could not be trusted —
    the valid partial data is kept, but no rate is calculated.
    """
    if runs < 1:
        raise Rejection(f"--runs must be a positive count, got {runs}")
    if rts_caps < 1:
        raise Rejection(
            f"--rts-caps must be a positive capability count, got {rts_caps}")
    script = resolve_probe(probe)
    descriptor = fetch_descriptor(probe, script)
    root = check_artifact_root(artifact_root or default_artifact_root())
    invocation = new_invocation_dir(root, probe)
    measurement = Measurement(probe, descriptor, runs, rts_caps, root, invocation)

    cursor = start_port
    with LiveRegistry() as registry:
        measurement.peak_concurrency = registry.peak
        for index in range(1, runs + 1):
            registry.sample()
            run_dir = invocation / f"run-{index:03d}"
            engine_dir = run_dir / "engine"
            engine_dir.mkdir(parents=True, exist_ok=True)
            events_path = run_dir / "events.jsonl"
            stdout_path = run_dir / "stdout.txt"
            # Created before launch so a probe that dies before writing
            # anything still leaves a readable (empty) stream rather
            # than an absent one.
            events_path.touch()

            lease, cursor = acquire_port(cursor)
            try:
                if announce:
                    announce(index, runs, lease.port)
                ok, timed_out, elapsed, out = run_probes.run_one(
                    script, lease.port, timeout, None,
                    event_path=str(events_path),
                    artifact_dir=str(run_dir),
                    engine_log_dir=str(engine_dir),
                    rts_caps=rts_caps)
            finally:
                # Held until run_one has returned, which is after it has
                # reaped the probe's whole process group — so the engine
                # has really let the port go before anyone else can
                # lease it.
                lease.release()

            stdout_path.write_text(out or "", encoding="utf-8")

            def stop_with_harness_error(detail: str) -> Measurement:
                """Record the untrustworthy run and end the measurement.

                The run is kept OUT of `runs` — a harness error is not a
                fourth probe outcome — but it is reported, with its
                retained artifact directory and whatever checks parsed
                cleanly, so the result never reads as "nothing went
                wrong, nothing retained".
                """
                measurement.status = "harness-error"
                measurement.error = f"run {index}: {detail}"
                measurement.error_run = RunRecord(
                    index, lease.port, RUN_HARNESS_ERROR, elapsed,
                    salvage_checks(descriptor, events_text), run_dir)
                registry.sample()
                measurement.peak_concurrency = registry.peak
                return measurement

            try:
                events_text = events_path.read_text(encoding="utf-8")
            except OSError as error:
                events_text = ""
                return stop_with_harness_error(
                    f"protocol event stream at {events_path} is unreadable "
                    f"({error})")

            try:
                outcome, outcomes = reconcile(descriptor, ok, timed_out,
                                              events_text, out or "")
            except HarnessError as error:
                return stop_with_harness_error(str(error))

            keep = outcome in (RUN_FAIL, RUN_TIMEOUT)
            if not keep:
                _remove_tree(run_dir)
            measurement.runs.append(RunRecord(
                index, lease.port, outcome, elapsed, outcomes,
                run_dir if keep else None))
            registry.sample()
        measurement.peak_concurrency = registry.peak
    return measurement


# --------------------------------------------------------------------------
# Reporting
# --------------------------------------------------------------------------
def render(measurement: Measurement) -> str:
    d = measurement.descriptor
    counts = measurement.check_counts()
    width = max(len(cid) for cid in d.ids)
    lines = [
        f"probe {measurement.probe} — {len(measurement.runs)}/"
        f"{measurement.requested_runs} runs, +RTS -N{measurement.rts_caps} -RTS",
        f"commit {measurement.commit_sha}  {measurement.timestamp}",
        "",
        f"  {'check':<{width}}  {'PASS':>5} {'FAIL':>5} {'MISS':>5}  label",
    ]
    for cid, label in d.checks:
        c = counts[cid]
        lines.append(
            f"  {cid:<{width}}  {c[probe_protocol.PASS]:>5} "
            f"{c[probe_protocol.FAIL]:>5} {c[probe_protocol.MISSING]:>5}  {label}")
    lines.append("")
    outcomes = ", ".join(f"{r.index}:{r.outcome}" for r in measurement.runs)
    lines.append(f"  runs: {outcomes or '(none)'}")
    if measurement.valid:
        rate = measurement.failure_rate or 0.0
        lines.append(
            f"  failures (incl. timeouts): {measurement.failure_count}/"
            f"{measurement.requested_runs} = {rate * 100:.1f}%   "
            f"timeouts: {measurement.timeout_count}")
    else:
        lines.append(f"  HARNESS ERROR: {measurement.error}")
        lines.append("  no trustworthy failure rate — the partial data above "
                     "is what parsed cleanly")
        broken = measurement.error_run
        if broken is not None:
            reported = ", ".join(f"{cid}={result}"
                                 for cid, result in broken.checks.items())
            lines.append(
                f"  run {broken.index} on port {broken.port} was discarded "
                f"after {broken.elapsed:.1f}s: {reported}")
    lines.append(
        f"  elapsed: total {measurement.total_elapsed:.1f}s, worst "
        f"{measurement.worst_elapsed:.1f}s   RTS capabilities: "
        f"{measurement.rts_caps}   peak concurrency: "
        f"{measurement.peak_concurrency}")
    retained = measurement.retained_artifacts()
    if retained:
        lines.append("  retained artifacts (unsuccessful runs):")
        lines.extend(f"    {path}" for path in retained)
    elif measurement.valid:
        lines.append("  retained artifacts: none (every run succeeded)")
    else:
        lines.append("  retained artifacts: none")
    return "\n".join(lines)


def write_result(measurement: Measurement, path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    Path(path).write_text(
        json.dumps(measurement.to_document(), indent=2, sort_keys=True) + "\n",
        encoding="utf-8")


# --------------------------------------------------------------------------
# CLI
# --------------------------------------------------------------------------
def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--probe", required=True,
                    help="exactly one registered, probe-result/v1 probe key")
    ap.add_argument("--runs", type=int, required=True,
                    help="how many times to run it (positive)")
    ap.add_argument("--result", default=None,
                    help="write the probe-flake-result/v1 document here")
    ap.add_argument("--artifact-root", default=None,
                    help=f"override the artifact root "
                         f"(default {default_artifact_root()})")
    ap.add_argument("--rts-caps", type=int, default=DEFAULT_RTS_CAPS,
                    help=f"RTS capabilities for every engine "
                         f"(default {DEFAULT_RTS_CAPS}); there is no fallback")
    args = ap.parse_args(argv)

    def announce(index: int, total: int, port: int) -> None:
        print(f"[{index}/{total}] {args.probe} on port {port} ...",
              file=sys.stderr, flush=True)

    try:
        measurement = measure(
            args.probe, args.runs,
            artifact_root=Path(args.artifact_root) if args.artifact_root else None,
            rts_caps=args.rts_caps, announce=announce)
    except Rejection as error:
        print(f"probe_flake: {error}", file=sys.stderr)
        return EXIT_REJECTED
    except PortExhausted as error:
        print(f"probe_flake: {error}", file=sys.stderr)
        return EXIT_NO_PORT

    print(render(measurement))
    if args.result:
        write_result(measurement, args.result)
        print(f"\nwrote {RESULT_SCHEMA} to {args.result}")
    # A valid measurement exits 0 whatever it observed: reporting a
    # flake rate is this tool's purpose, so a nonzero rate is a result,
    # not a failure of the harness.
    return EXIT_OK if measurement.valid else EXIT_HARNESS_ERROR


if __name__ == "__main__":
    sys.exit(main())
