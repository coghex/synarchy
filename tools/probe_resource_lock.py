#!/usr/bin/env python3
"""Cross-process shared/exclusive probe resources (#1436).

`probe_runner_resources.ResourceLedger` is a reader/writer lock over the probes
inside ONE runner process — its own docstring says so, and the scheduler
owns it from a single thread. That is the whole coordination that exists
today, and it coordinates nobody else: a `/deflake` measurement and a
`tools/run_probes.py` sweep are independent processes, so the ledger in
each is blind to the other. Both drive the SAME checkout, so
`config_state_probe.py` taking `repo-config` exclusively cannot stop a
foreign engine booting into the tracked `config/` tree it is asserting
against.

This module is the missing half: the same reader/writer model, between
processes.

    hold = probe_resource_lock.acquire(
        exclusive=probe_runner_resources.exclusive_resources(key),
        shared=probe_runner_resources.shared_resources(key),
        namespace=probe_runner_resources.resource_namespace())
    try:
        ...run the probe...
    finally:
        hold.release()

What the interests mean is NOT redefined here. `run_probes` owns the two
declaration tables and the two accessors; this module is told a set of
names and an interest, and coordinates them. It deliberately imports
nothing from the rest of `tools/`, which is what lets `run_probes`
import it: `probe_flake` and `probe_claim` both import `run_probes`, so
anything reaching back into them from here would close an import cycle.
The two consequences of that are called out where they bite — the
scratch-directory checks below, and `repository_namespace`.

The lock IS the flock
---------------------
One file per (namespace, resource); a SHARED interest is
`flock(LOCK_SH)` on it and an EXCLUSIVE interest is `flock(LOCK_EX)`.
The kernel's reader/writer semantics are exactly the model
`ResourceLedger` implements by hand, so nothing here counts holders or
decides staleness — the two operations a hand-rolled protocol gets
wrong. A holder that is SIGKILLed, or that segfaults, releases
everything it held the moment it dies, because an flock lives on the
open file description and dies with it. That is also why waiting for a
resource cannot wedge: there is no such thing as a stale holder to
recover from.

Lock files are never unlinked. `probe_flake.PortLease` records the
reason at length and it is the same one: unlinking is precisely the
operation that reintroduces the race the lock removed, because a second
process can create a fresh file at the same name and lock THAT while the
first still holds the old inode.

Where the files live, and why flat in `/tmp`
--------------------------------------------
`probe_flake._machine_wide_scratch` establishes the arrangement and the
reasoning transfers unchanged: a dedicated subdirectory is owned by
whichever account created it, and a directory's owner may unlink entries
inside it however the sticky bit is set — so that account could remove a
held lock's pathname, create a new file at the same name, lock that, and
hand one exclusive resource to two processes. `/tmp` itself is root-owned
and sticky on every supported platform, so no unprivileged account owns
the namespace. `_check_shared_dir` verifies that rather than assuming
it, and repairs nothing: a shared directory is not ours to chmod.

`probe_flake` performs the identical three checks for the identical
reason, and the duplication is forced rather than chosen: `probe_flake`
imports `run_probes`, `run_probes` imports this module, so importing its
helpers here would be a cycle. Neither copy may be relaxed without the
other.

The namespace is the REPOSITORY, not the worktree
-------------------------------------------------
`probe_engine.REPO_ROOT` is derived from the tools file's own location, so
every linked worktree resolves a different value and two worktrees of one
repository would namespace separately — while both drive the same
tracked `config/` tree through the same build directory, which is the
conflict the resource declarations exist to describe. The namespace is
therefore the repository's COMMON git directory, which every linked
worktree of one repository resolves identically and no worktree of
another repository can collide with. `probe_claim.repository_claim_root`
resolves the same `--git-common-dir` for the same reason; again the two
cannot share a helper without a cycle.

The path is hashed rather than embedded, so a lock file name is a fixed
length whatever the checkout is called and holds no path separators.

Acquisition is ALL-OR-NOTHING and never waits while holding
-----------------------------------------------------------
`acquire` takes the whole interest set or none of it. The set is taken
in sorted name order and every attempt is non-blocking, so no process
ever holds one resource while waiting for another and a deadlock cycle
cannot form. A conflict rolls back everything already taken and raises
`ResourceBusy`, naming the resource, the interest that was refused, and
whatever live holders could be identified.

`wait_acquire` is the polling wrapper for a caller that must eventually
run (the sequential probe runner); `acquire` is what a caller that must
report instead of waiting uses (`/deflake`, and the parallel scheduler's
dispatch attempt).

Release is ownership-safe by construction
-----------------------------------------
Releasing drops OUR file descriptors and nothing else — an flock is held
by an open file description, so there is no way to express "release
someone else's". A late cleanup after a successor has taken the resource
therefore cannot take it away from them. The holder note a process
publishes for diagnostics carries a uuid, so it cannot unlink a
successor's note either.

Holder notes are DIAGNOSTICS ONLY
---------------------------------
Mutual exclusion is the flock above and nothing else. Beside it, a
holder publishes a small flat note naming itself and what it holds, so a
refused acquirer can say who is in the way instead of only that someone
is. A note is live while its own flock is held, never by the pid it
records — after an abnormal exit the operating system may reissue that
number to an unrelated process. The note is locked BEFORE it is named,
then published with `os.replace`, which closes the window in which a
concurrent scan could see an unlocked entry and reap it;
`probe_flake.LiveRegistry` publishes its registration the same way. If
any of this fails the acquisition still succeeds: losing a diagnostic is
not a reason to refuse a resource the kernel already granted.
"""
from __future__ import annotations

import contextlib
import errno
import fcntl
import hashlib
import json
import os
import re
import socket
import stat
import subprocess
import time
import uuid
from pathlib import Path

# The host-shared scratch directory, and the flat-file prefix inside it.
# `/tmp` itself, for the ownership reason in the module docstring — not
# `tempfile.gettempdir()`, which follows `TMPDIR` and is per-session on
# macOS, and not a subdirectory of either.
LOCK_ROOT = Path("/tmp")
SHARED_PREFIX = "synarchy-probe-resource"

# `/tmp`'s own mode: writable by everyone, entries removable only by
# their creator. Exactly what a cross-user lock namespace needs.
SHARED_DIR_MODE = 0o1777
SHARED_FILE_MODE = 0o666

# The two interests. Spelled here so a diagnostic and a caller cannot
# disagree about the words.
SHARED = "shared"
EXCLUSIVE = "exclusive"

# How long a holder note may be unlocked before a scanner reaps it. The
# note is locked before it is named and published atomically, so an
# unlocked note is an abandoned one; the grace covers only the case
# where a scan catches a file whose owner died between the two, and it
# costs nothing but a stale diagnostic line.
NOTE_REAP_GRACE_SECONDS = 30.0

# How often `wait_acquire` retries, and how often it repeats itself.
DEFAULT_POLL_SECONDS = 1.0
DEFAULT_ANNOUNCE_SECONDS = 30.0

# A resource name is a table key in `run_probes`, but it becomes part of
# a filename in a world-writable directory, so it is validated rather
# than trusted: no separators, no leading dot, nothing that could walk
# out of `LOCK_ROOT`.
_NAME_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$")


class ResourceLockError(Exception):
    """A controlled refusal: nothing was acquired, nothing was created."""


class ResourceBusy(Exception):
    """The interest set could not be granted; everything taken was released.

    Carries the ONE resource and interest that was refused — the first
    in sorted order, so the report is deterministic — beside whatever
    live holders could be identified. `holders` is best effort: it is
    read from the diagnostic notes, which a holder may legitimately have
    failed to publish, so an empty list means "nobody could be named",
    never "nobody holds it".
    """

    def __init__(self, resource: str, interest: str, *, namespace: str,
                 holders: list | None = None):
        self.resource = resource
        self.interest = interest
        self.namespace = namespace
        self.holders = list(holders or ())
        super().__init__(self.describe())

    def describe(self) -> str:
        who = ", ".join(
            f"{holder.get('owner') or 'an unidentified process'}"
            f" ({holder.get('interest', '?')})"
            for holder in self.holders)
        detail = f"; held by {who}" if who else ""
        return (f"the {self.interest} interest in resource "
                f"{self.resource!r} is not available{detail}")

    def to_document(self) -> dict:
        return {
            "resource": self.resource,
            "interest": self.interest,
            "namespace": self.namespace,
            "holders": self.holders,
        }


# --------------------------------------------------------------------------
# The shared scratch directory
# --------------------------------------------------------------------------
def _check_shared_dir(path: Path, uid: int | None = None) -> Path:
    """Validate the host-shared scratch directory; never repair it.

    The same three properties `probe_flake._check_shared_dir` verifies,
    for the same reason, and neither copy may be relaxed alone:

    * STICKY, so an entry is removable only by its own creator;
    * OWNED BY ROOT OR BY US, so no other unprivileged account can
      unlink a held lock's pathname and recreate it — the one hole a
      merely-sticky, user-owned directory leaves open;
    * WRITABLE by us, or no lock can be taken at all.

    It is created only when absent, which in practice happens only under
    a redirected root.
    """
    uid = os.getuid() if uid is None else uid
    try:
        if not path.exists():
            path.mkdir(mode=SHARED_DIR_MODE, parents=True, exist_ok=True)
        # FOLLOWS a link deliberately and judges what it lands on: `/tmp`
        # IS a symlink to `/private/tmp` on macOS, so refusing links
        # outright would refuse the one path this is built around.
        # Nothing is trusted because of how it was reached — the checks
        # below apply to the real directory. Lock FILES are a different
        # matter and are still opened `O_NOFOLLOW`.
        info = path.stat()
    except OSError as error:
        raise ResourceLockError(
            f"could not use the resource-lock directory {path} "
            f"({error})") from None
    if not stat.S_ISDIR(info.st_mode):
        raise ResourceLockError(
            f"the resource-lock path {path} is not a directory")
    mode = stat.S_IMODE(info.st_mode)
    if not mode & stat.S_ISVTX:
        raise ResourceLockError(
            f"the resource-lock directory {path} is mode {mode:04o}, which "
            f"is not sticky; any local user could then replace another "
            f"process's lock file and hand one exclusive resource to two "
            f"probes")
    if info.st_uid not in (0, uid):
        raise ResourceLockError(
            f"the resource-lock directory {path} is owned by uid "
            f"{info.st_uid}, which is neither root nor this user; that "
            f"account could unlink a held lock and recreate it, so the lock "
            f"would stop meaning anything")
    if not os.access(path, os.W_OK | os.X_OK):
        raise ResourceLockError(
            f"the resource-lock directory {path} is not writable by this "
            f"user; probe resources cannot be coordinated")
    return path


def _open_shared_lock_file(path: Path, flags: int) -> int | None:
    """Open a lock file in the shared directory, or None if it is unsafe.

    `O_NOFOLLOW` is load-bearing rather than hygiene: the directory is
    world-writable by design, so any local user can plant a symlink at
    an unused resource's lock name pointing at a file we may write.
    The regular-file and link-count checks close the same hole for a
    planted hard link. Every failure here means "this lock is not
    available to us" — which the caller reports as a conflict, the safe
    direction — and never a silent success.
    """
    try:
        fd = os.open(path, flags | os.O_NOFOLLOW, SHARED_FILE_MODE)
    except OSError:
        return None
    try:
        info = os.fstat(fd)
    except OSError:
        with contextlib.suppress(OSError):
            os.close(fd)
        return None
    if not stat.S_ISREG(info.st_mode) or info.st_nlink != 1:
        with contextlib.suppress(OSError):
            os.close(fd)
        return None
    return fd


def _share_file(fd: int) -> None:
    """Let any local user open this lock file; ignore it if we cannot.

    A lock nobody else can open coordinates nobody else. Failing is
    expected when the file belongs to another account — they already
    made it shareable.
    """
    with contextlib.suppress(OSError):
        os.fchmod(fd, SHARED_FILE_MODE)


# --------------------------------------------------------------------------
# Identity
# --------------------------------------------------------------------------
def require_name(value, what: str) -> str:
    """A single path-safe component, or a controlled refusal."""
    if not isinstance(value, str) or not _NAME_RE.match(value):
        raise ResourceLockError(
            f"{what} must be a short name of letters, digits, '.', '_' or "
            f"'-' beginning with a letter or digit, got {value!r}")
    return value


def repository_common_dir(repo_root=None) -> Path:
    """The repository's common git directory, absolute and resolved.

    `--git-common-dir` rather than `--git-dir` is the load-bearing
    choice, and it is the one `probe_claim.repository_claim_root` makes
    too: in a linked worktree the latter names that worktree's private
    `.git/worktrees/<name>`, so every worktree would namespace
    separately and the lock would coordinate nobody.

    A checkout git cannot answer for is a controlled refusal rather than
    a fall back to the directory path. A path-derived namespace is
    exactly the split this resolves, so inventing one when git is
    unavailable would silently reintroduce it in the one situation
    nobody would think to check.
    """
    root = Path(repo_root or ".")
    try:
        done = subprocess.run(["git", "rev-parse", "--git-common-dir"],
                              cwd=str(root), text=True, capture_output=True,
                              timeout=30)
    except (OSError, subprocess.SubprocessError) as error:
        raise ResourceLockError(
            f"could not resolve the repository's common git directory from "
            f"{root} ({error})") from None
    if done.returncode != 0:
        raise ResourceLockError(
            f"could not resolve the repository's common git directory from "
            f"{root}: {(done.stderr or '').strip()}")
    answer = done.stdout.strip()
    if not answer:
        raise ResourceLockError(
            f"`git rev-parse --git-common-dir` answered nothing in {root}")
    common = Path(answer)
    if not common.is_absolute():
        common = root / common
    return common.resolve()


def repository_namespace(repo_root=None) -> str:
    """A stable token naming THE REPOSITORY every worktree of it shares.

    Hashed rather than embedded so the token is a fixed length, holds no
    path separator, and cannot leak a checkout's location into a
    world-readable filename.
    """
    common = repository_common_dir(repo_root)
    return hashlib.sha256(str(common).encode("utf-8")).hexdigest()[:16]


def lock_path(resource: str, *, namespace: str, root: Path | None = None) -> Path:
    """The one lock file for `resource` in `namespace`."""
    base = Path(root) if root is not None else LOCK_ROOT
    return base / (f"{SHARED_PREFIX}-{require_name(namespace, 'a namespace')}"
                   f"-res-{require_name(resource, 'a resource name')}")


def _note_glob(namespace: str) -> str:
    return f"{SHARED_PREFIX}-{namespace}-holder-*.json"


def _owner_description() -> str:
    return f"{os.environ.get('USER') or 'unknown'}@{socket.gethostname()}"


# --------------------------------------------------------------------------
# Holder notes: diagnostics, never mutual exclusion
# --------------------------------------------------------------------------
def _note_is_live(path: Path) -> bool:
    """True while some process still holds this note.

    Liveness is the LOCK, never the pid the note records: after an
    abnormal termination the operating system may hand that number to an
    unrelated process, and a pid-and-age test would then read an
    abandoned note as live forever. An abandoned note is reaped here,
    while its lock is held, so recovery is safe rather than timed — but
    only once it is older than `NOTE_REAP_GRACE_SECONDS`, which covers
    the one moment a note legitimately exists unlocked: between the
    `os.replace` that publishes it and the death of a process that never
    got to lock it.
    """
    fd = _open_shared_lock_file(path, os.O_RDONLY)
    if fd is None:
        # Gone between the scan and here, or not a plain file we may
        # safely touch — either way no live holder is described by it.
        return False
    try:
        try:
            fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except OSError:
            # Still held — by another process, or by another note object
            # in this one, since flock conflicts between open file
            # descriptions rather than between processes.
            return True
        try:
            age = time.time() - os.fstat(fd).st_mtime
        except OSError:
            age = NOTE_REAP_GRACE_SECONDS + 1.0
        if age >= NOTE_REAP_GRACE_SECONDS:
            with contextlib.suppress(OSError):
                path.unlink()
        with contextlib.suppress(OSError):
            fcntl.flock(fd, fcntl.LOCK_UN)
        return False
    finally:
        with contextlib.suppress(OSError):
            os.close(fd)


def holders(resource: str, *, namespace: str, root: Path | None = None) -> list:
    """Every live holder of `resource` that could be identified.

    Best effort by construction: a holder whose note could not be
    published, or was reaped, is simply absent. An empty list therefore
    means "nobody could be named", never "nobody holds it" — the flock
    is the authority on that and this is only how a refusal explains
    itself.
    """
    require_name(resource, "a resource name")
    require_name(namespace, "a namespace")
    base = Path(root) if root is not None else LOCK_ROOT
    found = []
    try:
        entries = sorted(base.glob(_note_glob(namespace)))
    except OSError:
        return found
    for entry in entries:
        try:
            document = json.loads(entry.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            continue
        if not isinstance(document, dict):
            continue
        interest = None
        if resource in (document.get("exclusive") or ()):
            interest = EXCLUSIVE
        elif resource in (document.get("shared") or ()):
            interest = SHARED
        if interest is None:
            continue
        if not _note_is_live(entry):
            continue
        found.append({
            "owner": document.get("owner"),
            "pid": document.get("pid"),
            "purpose": document.get("purpose"),
            "interest": interest,
            "acquired": document.get("acquired"),
        })
    return found


class _Note:
    """One holder's published description of what it holds."""

    def __init__(self, namespace: str, root: Path):
        self.path = (root / f"{SHARED_PREFIX}-{namespace}-holder-"
                            f"{os.getpid()}-{uuid.uuid4().hex[:8]}.json")
        self._fd: int | None = None

    def publish(self, payload: dict) -> None:
        """Lock, fill and name the note. Never raises.

        Locked BEFORE it is named and published with `os.replace`, so a
        concurrent scan can never see an unlocked, complete note that is
        about to become live. The staging name does not end in `.json`,
        so a scan skips it, and the lock follows the inode through the
        rename.

        A failure anywhere here loses a diagnostic and nothing else: the
        resources are already held by the kernel, and refusing them over
        an unwritable note would be trading the feature for its
        commentary.
        """
        staging = self.path.with_suffix(".staging")
        try:
            # O_EXCL, not merely O_NOFOLLOW: the name carries a uuid, so
            # this process is the only thing that can legitimately create
            # it, which rules out a planted file outright.
            fd = os.open(staging,
                         os.O_CREAT | os.O_EXCL | os.O_RDWR | os.O_NOFOLLOW,
                         SHARED_FILE_MODE)
        except OSError:
            return
        _share_file(fd)
        try:
            fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
            os.write(fd, json.dumps(payload, sort_keys=True).encode("utf-8"))
            os.replace(staging, self.path)
        except (OSError, ValueError, TypeError):
            with contextlib.suppress(OSError):
                os.close(fd)
            with contextlib.suppress(OSError):
                staging.unlink()
            return
        self._fd = fd

    def withdraw(self) -> None:
        """Unlink and unlock this note. Never raises.

        The name carries a uuid, so this can only ever remove OUR note —
        a successor's note is a different name and is left alone.
        """
        if self._fd is None:
            return
        with contextlib.suppress(OSError):
            self.path.unlink()
        with contextlib.suppress(OSError):
            fcntl.flock(self._fd, fcntl.LOCK_UN)
        with contextlib.suppress(OSError):
            os.close(self._fd)
        self._fd = None


# --------------------------------------------------------------------------
# The hold
# --------------------------------------------------------------------------
class ResourceHold:
    """Held cross-process interests. Releasing drops OUR descriptors only."""

    def __init__(self, namespace: str, exclusive: frozenset,
                 shared: frozenset, fds: dict, note: "_Note | None"):
        self.namespace = namespace
        self.exclusive = exclusive
        self.shared = shared
        self._fds = fds
        self._note = note
        self.released = False

    def __enter__(self) -> "ResourceHold":
        return self

    def __exit__(self, exc_type, exc, tb) -> bool:
        self.release()
        return False

    def interest(self, resource: str) -> str | None:
        if resource in self.exclusive:
            return EXCLUSIVE
        if resource in self.shared:
            return SHARED
        return None

    def release(self) -> None:
        """Give every held interest back. Idempotent, and never raises.

        There is no way for this to take a resource away from a
        successor: an flock is held by an open file description, so
        unlocking ours says nothing about anyone else's, and the lock
        files themselves are never unlinked. A late cleanup after this
        process's work was abandoned is therefore harmless by
        construction rather than by a token check.
        """
        if self.released:
            return
        self.released = True
        if self._note is not None:
            self._note.withdraw()
            self._note = None
        for fd in self._fds.values():
            with contextlib.suppress(OSError):
                fcntl.flock(fd, fcntl.LOCK_UN)
            with contextlib.suppress(OSError):
                os.close(fd)
        self._fds = {}

    def to_document(self) -> dict:
        return {
            "namespace": self.namespace,
            "exclusive": sorted(self.exclusive),
            "shared": sorted(self.shared),
            "released": self.released,
        }


def _normalize(exclusive, shared) -> tuple[frozenset, frozenset]:
    """The two interest sets, validated and made disjoint.

    `probe_runner_resources.shared_resources` already subtracts what a probe
    declares exclusively, so an overlap should not reach here. It is
    resolved rather than trusted, and resolved toward EXCLUSIVE, because
    the alternative — taking both interests on one name — would mean
    acquiring the same lock file twice from one process, which flock
    refuses against our own open description and would deadlock a
    blocking caller against itself.
    """
    exclusive = frozenset(
        require_name(name, "a resource name") for name in (exclusive or ()))
    shared = frozenset(
        require_name(name, "a resource name") for name in (shared or ()))
    return exclusive, shared - exclusive


def acquire(*, exclusive=(), shared=(), namespace: str,
            root: Path | None = None, purpose: str = "") -> ResourceHold:
    """Take the whole interest set, or none of it. Never waits.

    Names are taken in sorted order and every attempt is non-blocking,
    so this can neither deadlock against another caller (nobody ever
    waits while holding) nor be starved into a partial acquisition: the
    first refusal rolls back everything already taken and raises
    `ResourceBusy`.

    An empty interest set is a legitimate hold that owns nothing, so a
    caller never has to branch on whether its probe declared anything.
    """
    require_name(namespace, "a namespace")
    want_exclusive, want_shared = _normalize(exclusive, shared)
    base = _check_shared_dir(Path(root) if root is not None else LOCK_ROOT)

    plan = sorted([(name, EXCLUSIVE) for name in want_exclusive] +
                  [(name, SHARED) for name in want_shared])
    fds: dict = {}

    def rollback() -> None:
        for fd in fds.values():
            with contextlib.suppress(OSError):
                fcntl.flock(fd, fcntl.LOCK_UN)
            with contextlib.suppress(OSError):
                os.close(fd)
        fds.clear()

    for name, interest in plan:
        path = lock_path(name, namespace=namespace, root=base)
        fd = _open_shared_lock_file(path, os.O_CREAT | os.O_RDWR)
        if fd is None:
            # Unopenable, a symlink, a hard link, or not a regular file
            # — in a world-writable directory each of those is a planted
            # object rather than a busy resource. It is a hard refusal
            # and NOT a conflict: `ResourceBusy` says "someone else is
            # working, come back later", which a caller reports as an
            # ordinary no-work success, and an unusable lock file is not
            # that. Exclusion cannot be established here, so nothing may
            # run and the diagnostic has to name why.
            rollback()
            raise ResourceLockError(
                f"the resource-lock file {path} is not a plain file this "
                f"process may safely lock, so a {interest} interest in "
                f"{name!r} cannot be established; nothing was acquired")
        _share_file(fd)
        operation = (fcntl.LOCK_EX if interest == EXCLUSIVE
                     else fcntl.LOCK_SH)
        try:
            fcntl.flock(fd, operation | fcntl.LOCK_NB)
        except OSError as error:
            with contextlib.suppress(OSError):
                os.close(fd)
            rollback()
            if error.errno not in (errno.EWOULDBLOCK, errno.EAGAIN,
                                   errno.EACCES):
                raise ResourceLockError(
                    f"could not lock {path} for a {interest} interest in "
                    f"{name!r} ({error})") from None
            raise ResourceBusy(name, interest, namespace=namespace,
                               holders=holders(name, namespace=namespace,
                                               root=base)) from None
        except BaseException:
            with contextlib.suppress(OSError):
                os.close(fd)
            rollback()
            raise
        fds[name] = fd

    note = None
    if plan:
        note = _Note(namespace, base)
        note.publish({
            "owner": _owner_description(),
            "pid": os.getpid(),
            "purpose": purpose or "",
            "exclusive": sorted(want_exclusive),
            "shared": sorted(want_shared),
            "acquired": time.time(),
        })
    return ResourceHold(namespace, want_exclusive, want_shared, fds, note)


def wait_acquire(*, exclusive=(), shared=(), namespace: str,
                 root: Path | None = None, purpose: str = "",
                 poll: float = DEFAULT_POLL_SECONDS,
                 announce=None,
                 announce_interval: float = DEFAULT_ANNOUNCE_SECONDS,
                 sleep=time.sleep) -> ResourceHold:
    """`acquire`, retried until it succeeds.

    For a caller that must eventually run the probe rather than report
    that it could not — the sequential probe runner. It is deliberately
    unbounded, and that is safe rather than optimistic: an flock dies
    with the open file description that holds it, so a crashed or killed
    holder releases everything instantly and there is no stale state to
    time out against. Only a live process still doing the work it
    declared can keep us here, which is the same wait
    `probe_runner_resources.ResourceLedger` already imposes within one
    sweep.

    Time spent here is the caller's to account for. Nothing about it is
    charged to the probe: the wait finishes before the probe process is
    launched, so a probe's own elapsed time and timeout cover execution
    alone.
    """
    announced = None
    while True:
        try:
            return acquire(exclusive=exclusive, shared=shared,
                           namespace=namespace, root=root, purpose=purpose)
        except ResourceBusy as busy:
            now = time.monotonic()
            if announce is not None and (announced is None or
                                         now - announced >= announce_interval):
                announce(busy)
                announced = now
            sleep(poll)
