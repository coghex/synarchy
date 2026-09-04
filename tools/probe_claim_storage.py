#!/usr/bin/env python3
"""The claim FILE: its namespace, its codec, and its durable storage (#2148).

The filesystem leaf of `tools/probe_claim.py`'s three owners. It knows
where a claim lives, what one is on disk, how to read one back without
trusting it, and how to put one there durably. It decides NOTHING about
ownership: whether a claim is live, whose it is, or whether an aged
file may be taken over is `probe_claim_lease.py`'s, which is why this
module raises `ClaimError` and never `ClaimDenied`.

What a claim is, exactly
------------------------
A claim is a FILE, created `O_CREAT|O_EXCL` at
`<git-common-dir>/probe-claims/<probe>.json`, carrying its owner, its
acquisition token and its lease. The FILE is the lock. That is a
deliberate departure from `probe_flake.PortLease`, which makes an
advisory `flock` the lease precisely so a dead holder owns nothing: a
claim must survive its holder's death, because an agent that was
SIGKILLed mid-measurement has left an engine's worth of ambiguity
behind and the probe must stay unavailable until the lease expires
rather than become instantly reclaimable.

A sidecar `flock` at `.<probe>.lock` exists all the same, and it is not
the claim. `serialized` holds it for the read-decide-write of the claim
file, which is what makes TAKEOVER single-successor: reclaiming a
lapsed claim is unavoidably a read-then-write, and two reclaimers doing
that unserialized is the classic race where the second one's write
lands on top of the first one's fresh claim. Held for microseconds per
operation, never across a measurement — so a crashed holder releases
the sidecar instantly and still holds its claim until the lease runs
out.

The namespace
-------------
`<git-common-dir>/probe-claims`, resolved from the REPOSITORY-common
git directory: every linked worktree of one repository resolves the
same directory, and it moves with none of the three things that would
split the namespace and let two agents both believe they own a probe —
the current worktree, `--artifact-root`, and `TMPDIR`. It is
repository-scoped rather than host-global on purpose; a port is a host
resource, a probe key is a repository's. Untracked per-repository
coordination state under the common git directory is the established
convention here, not a new one: `tools/probe_inflight.py` reads the
`$test` coordinator's `<git-common-dir>/codex-test` tree the same way.

Timestamps carry MICROSECONDS, because whole-second stamps round a
lease down: a sub-second lease would be born already expired and any
lease could lapse a second early. `parse_stamp` still accepts the
legacy second-precision rendering, because a claim file is transient
untracked scratch state a previous build may have written.

Malformed, incomplete and inconsistent claims
---------------------------------------------
`read_payload` returns a payload only for a claim that parses AND
carries a COMPLETE, well-typed, self-consistent claim naming the probe
the caller asked about. Everything else comes back as `None` beside the
file's own mtime, which is what the lease owner ages out against
instead of trusting: a crash between the exclusive create and the
payload write lands there, and must not open a window in which a
competing acquisition succeeds immediately.

Completeness is checked against every field a claim carries, not merely
the ones an ownership decision reads: a partial file holding nothing but
a probe, a token and a far-future expiry would otherwise read as a live
claim forever — never aged out, because ageing out is what happens to
malformed claims — and one stray write would wedge the probe
permanently.

CONSISTENCY is checked alongside completeness, because well-typed
fields can still contradict each other. A one-second `lease_seconds`
beside an `expires_at` years away is a file that never expires and
never ages out either; so the lease has to be the distance between the
two timestamps that describe it, within exactly the rounding a legacy
second-precision file carries (`STAMP_TOLERANCE_SECONDS`).

Not a program. `tools/probe_claim.py` remains the only command.
"""
from __future__ import annotations

import contextlib
import errno
import fcntl
import json
import math
import os
import stat
import subprocess
import sys
import tempfile
from datetime import datetime, timedelta, timezone
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_engine  # noqa: E402

CLAIM_SCHEMA = "probe-claim/v1"

# The claim namespace, under the repository-common git directory.
CLAIM_DIR_NAME = "probe-claims"
LOCK_PREFIX = "."
LOCK_SUFFIX = ".lock"
STAGING_PREFIX = ".probe_claim."
STAGING_SUFFIX = ".tmp"

# The upper bound, and it is not arbitrary. `timedelta` overflows well
# below the float range — `timedelta(seconds=1e100)` raises — and the
# census `claim` definition caps `lease_seconds` at 1e9, so a lease
# above that would be refused when the acquisition was RECORDED, after
# the probe had already been claimed. One billion seconds is thirty-one
# years, which is not a short leash; anything past it is a typo or a
# unit mistake, and both are better refused before anything is claimed.
MAX_LEASE_SECONDS = 1_000_000_000.0

# How far a claim's own timestamps may disagree with its lease before it
# is treated as malformed. Not slack for sloppy writers: it is exactly
# the rounding a SECOND-precision file carries, since `renewed_at` and
# `expires_at` were each truncated independently before this module
# started writing microseconds. Anything past it is a file whose fields
# contradict each other.
STAMP_TOLERANCE_SECONDS = 2.0

CLAIM_FILE_MODE = 0o600
CLAIM_DIR_MODE = 0o700


class ClaimError(Exception):
    """A controlled refusal: nothing was claimed, nothing was created."""


# --------------------------------------------------------------------------
# The namespace
# --------------------------------------------------------------------------
def repository_claim_root(repo_root: str | None = None) -> Path:
    """`<git-common-dir>/probe-claims`, the one namespace for this repository.

    `--git-common-dir` rather than `--git-dir` is the load-bearing
    choice: in a linked worktree the latter names that worktree's
    private `.git/worktrees/<name>` directory, so every agent would get
    a namespace of its own and the claim would coordinate nobody. The
    former names the main repository's `.git` from every worktree of it.

    The result is resolved to an absolute path against the checkout it
    was asked about, because git may answer with a relative one.
    """
    root = Path(repo_root or probe_engine.REPO_ROOT)
    try:
        done = subprocess.run(["git", "rev-parse", "--git-common-dir"],
                              cwd=str(root), text=True, capture_output=True,
                              timeout=30)
    except (OSError, subprocess.SubprocessError) as error:
        raise ClaimError(
            f"could not resolve the repository's common git directory from "
            f"{root} ({error})") from None
    if done.returncode != 0:
        raise ClaimError(
            f"could not resolve the repository's common git directory from "
            f"{root}: {(done.stderr or '').strip()}")
    answer = done.stdout.strip()
    if not answer:
        raise ClaimError(
            f"`git rev-parse --git-common-dir` answered nothing in {root}")
    common = Path(answer)
    if not common.is_absolute():
        common = (root / common)
    return common.resolve() / CLAIM_DIR_NAME


def ensure_root(root: Path) -> Path:
    """The claim directory, created if absent; never a symlink.

    A symlinked namespace is refused rather than followed: the claim
    files are the coordination state, and a link is the one way they
    could quietly end up somewhere another worktree does not look.
    """
    if root.is_symlink():
        raise ClaimError(
            f"refusing to use the claim directory {root}: it may not be a "
            f"symlink")
    try:
        root.mkdir(mode=CLAIM_DIR_MODE, parents=True, exist_ok=True)
    except OSError as error:
        raise ClaimError(
            f"could not create the claim directory {root} ({error})") from None
    if not root.is_dir():
        raise ClaimError(f"the claim path {root} is not a directory")
    return root


def claim_path(probe: str, root: Path) -> Path:
    return root / f"{probe}.json"


def lock_file_path(probe: str, root: Path) -> Path:
    return root / f"{LOCK_PREFIX}{probe}{LOCK_SUFFIX}"


# --------------------------------------------------------------------------
# Time
# --------------------------------------------------------------------------
MICROSECOND_STAMP = "%Y-%m-%dT%H:%M:%S.%fZ"
SECOND_STAMP = "%Y-%m-%dT%H:%M:%SZ"


def utc_now() -> datetime:
    return datetime.now(timezone.utc)


def stamp(moment: datetime) -> str:
    """A claim-file timestamp, to the MICROSECOND.

    The precision is load-bearing rather than cosmetic. Every lease
    decision is a comparison against a parsed `expires_at`, so rounding
    the stored value to whole seconds quietly rounds the lease itself:
    a sub-second lease would be born already expired, and any lease
    would lapse up to a second early. Nothing may depend on the rounding
    being generous.
    """
    return moment.astimezone(timezone.utc).strftime(MICROSECOND_STAMP)


def stamp_second(moment: datetime) -> str:
    """The same instant to the second, for the census's `timestamp_utc`.

    The census schema declares whole-second timestamps and this is the
    one place a claim crosses into it. It is a RENDERING of the claim's
    own full-precision instant, never a second clock reading.
    """
    return moment.astimezone(timezone.utc).strftime(SECOND_STAMP)


def parse_stamp(value):
    """A stored `...Z` timestamp as an aware datetime, or None.

    Both renderings above are accepted: a claim file is transient
    untracked scratch state that a previous build may have written to
    the second, and the two are the same instant to within the
    precision each carries.

    None rather than an exception: an unparseable timestamp makes the
    claim MALFORMED, which has its own defined handling, and must not
    become a traceback out of an acquisition.
    """
    if not isinstance(value, str):
        return None
    for pattern in (MICROSECOND_STAMP, SECOND_STAMP):
        try:
            return datetime.strptime(value, pattern).replace(
                tzinfo=timezone.utc)
        except ValueError:
            continue
    return None


# --------------------------------------------------------------------------
# The claim file
# --------------------------------------------------------------------------
def commit_sha(repo_root: str | None = None) -> str:
    """This checkout's HEAD, or `unknown` when git could not be consulted.

    Resolved against the CLAIMING worktree rather than a module-level
    repository root: several worktrees of one repository share the claim
    namespace, and each is at its own commit.
    """
    root = str(Path(repo_root or probe_engine.REPO_ROOT))
    try:
        done = subprocess.run(["git", "rev-parse", "HEAD"], cwd=root,
                              text=True, capture_output=True, timeout=30)
    except (OSError, subprocess.SubprocessError):
        return "unknown"
    return done.stdout.strip() if done.returncode == 0 else "unknown"



def _encode(payload: dict) -> bytes:
    return (json.dumps(payload, indent=2, sort_keys=True) + "\n").encode("utf-8")


def read_payload(path: Path, expected: str | None = None):
    """`(payload_or_None, mtime_or_None)` for the claim at `path`.

    A payload is returned only when it parses AND carries a COMPLETE,
    well-typed claim naming the probe the caller asked about. Anything
    else — absent, empty, truncated, not an object, missing or
    ill-typed in any required field, carrying an unparseable timestamp,
    or naming a DIFFERENT probe — comes back as `None`, which is the
    MALFORMED case the caller ages out against the filesystem rather
    than trusting.

    Completeness is the load-bearing word, and checking only the fields
    an ownership decision happens to READ is the trap. A partial file
    carrying nothing but `probe`, `token` and a far-future `expires_at`
    would then read as a perfectly live claim forever: it is never aged
    out, because ageing out is what happens to MALFORMED claims, and a
    stray or truncated write would wedge the probe permanently. Every
    field `probe_claim_lease._payload` writes is therefore required
    here, so a partial write is recognized as partial whatever it
    happens to contain.

    A file that disagrees about its own identity is a copied or
    hand-edited claim, and honouring it would key the lock on the
    filename while reporting somebody else's owner.
    """
    try:
        info = os.lstat(path)
    except FileNotFoundError:
        return None, None
    except OSError as error:
        raise ClaimError(f"could not stat the claim {path} ({error})") from None
    if stat.S_ISLNK(info.st_mode):
        raise ClaimError(
            f"refusing to use the claim {path}: it may not be a symlink")
    if not stat.S_ISREG(info.st_mode):
        raise ClaimError(
            f"refusing to use the claim {path}: it must be a regular file "
            f"(got mode {stat.S_IFMT(info.st_mode):#o})")
    mtime = info.st_mtime
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, ValueError):
        return None, mtime
    if not isinstance(document, dict):
        return None, mtime
    if document.get("schema") != CLAIM_SCHEMA:
        return None, mtime
    for field in ("probe", "token", "owner", "host", "worktree"):
        value = document.get(field)
        if not isinstance(value, str) or not value:
            return None, mtime
    if expected is not None and document["probe"] != expected:
        return None, mtime
    # `isinstance(True, int)` is True, so a boolean pid has to be ruled
    # out explicitly rather than by type alone.
    pid = document.get("pid")
    if isinstance(pid, bool) or not isinstance(pid, int) or pid < 0:
        return None, mtime
    lease = document.get("lease_seconds")
    if (isinstance(lease, bool) or not isinstance(lease, (int, float))
            or not math.isfinite(lease) or not 0 < lease <= MAX_LEASE_SECONDS):
        return None, mtime
    stamps = {}
    for field in ("acquired_at", "renewed_at", "expires_at"):
        stamps[field] = parse_stamp(document.get(field))
        if stamps[field] is None:
            return None, mtime
    # A claim whose own fields contradict each other is malformed too,
    # and this is the shape that would otherwise wedge a probe FOREVER:
    # `lease_seconds: 1` beside an `expires_at` years away is a file
    # that never expires and never ages out, because ageing out is what
    # happens to malformed claims. Every field being individually
    # well-typed is not enough — the lease has to be the distance
    # between the two timestamps that describe it.
    if stamps["acquired_at"] > stamps["renewed_at"] + timedelta(
            seconds=STAMP_TOLERANCE_SECONDS):
        return None, mtime
    try:
        implied = stamps["renewed_at"] + timedelta(seconds=lease)
    except (OverflowError, OSError, ValueError):
        return None, mtime
    if abs((stamps["expires_at"] - implied).total_seconds()) > (
            STAMP_TOLERANCE_SECONDS):
        return None, mtime
    return document, mtime


def install(path: Path, payload: dict) -> None:
    """Write `payload` as the claim at `path`, replacing any prior one.

    Used only for TAKEOVER and RENEWAL, both of which run under the
    sidecar lock and both of which already hold the right to write here.
    A fresh acquisition uses `O_EXCL` instead, so the create itself is
    the exclusion.
    """
    body = _encode(payload)
    fd, staged = tempfile.mkstemp(dir=str(path.parent), prefix=STAGING_PREFIX,
                                  suffix=STAGING_SUFFIX)
    staged_path = Path(staged)
    try:
        with os.fdopen(fd, "wb") as handle:
            handle.write(body)
            handle.flush()
            os.fsync(handle.fileno())
        os.chmod(staged_path, CLAIM_FILE_MODE)
        os.replace(str(staged_path), str(path))
    except BaseException:
        with contextlib.suppress(OSError):
            staged_path.unlink()
        raise
    # The rename itself, made durable. A claim lost to a machine crash
    # would only fail in the SAFE direction — the probe becomes
    # available again — but a takeover that survives while the claim it
    # installed does not is the one ordering worth closing.
    with contextlib.suppress(OSError):
        dir_fd = os.open(str(path.parent), os.O_RDONLY)
        try:
            os.fsync(dir_fd)
        finally:
            os.close(dir_fd)


def create_exclusive(path: Path, payload: dict) -> bool:
    """`O_CREAT|O_EXCL` the claim into existence. False if it already exists."""
    body = _encode(payload)
    try:
        fd = os.open(str(path),
                     os.O_CREAT | os.O_EXCL | os.O_WRONLY | os.O_NOFOLLOW,
                     CLAIM_FILE_MODE)
    except FileExistsError:
        return False
    except OSError as error:
        if error.errno in (errno.ELOOP, errno.EEXIST):
            return False
        raise ClaimError(
            f"could not create the claim {path} ({error})") from None
    try:
        with os.fdopen(fd, "wb") as handle:
            handle.write(body)
            handle.flush()
            os.fsync(handle.fileno())
    except OSError as error:
        # An exclusive create that could not be filled in leaves an
        # empty claim, which is the MALFORMED case: occupied until it
        # ages out, never silently available.
        raise ClaimError(
            f"could not write the claim {path} ({error})") from None
    return True


@contextlib.contextmanager
def serialized(probe: str, root: Path):
    """Hold the sidecar `flock` for one claim-file operation.

    NOT the claim. It exists only so that reading the current claim,
    deciding, and writing cannot interleave with another agent doing the
    same — which is what makes a lapsed claim yield exactly one
    successor. It is taken and dropped within a single operation and is
    never held across a measurement, so a killed holder frees it at once
    while its CLAIM stands until the lease expires.
    """
    guard = lock_file_path(probe, root)
    if guard.is_symlink():
        raise ClaimError(
            f"refusing to use the claim lock {guard}: it may not be a symlink")
    try:
        fd = os.open(str(guard), os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
    except OSError as error:
        raise ClaimError(
            f"could not open the claim lock {guard} ({error})") from None
    try:
        info = os.fstat(fd)
        if not stat.S_ISREG(info.st_mode) or info.st_nlink != 1:
            raise ClaimError(
                f"refusing to use the claim lock {guard}: it must be a "
                f"regular file with exactly one link")
        fcntl.flock(fd, fcntl.LOCK_EX)
        try:
            yield
        finally:
            with contextlib.suppress(OSError):
                fcntl.flock(fd, fcntl.LOCK_UN)
    finally:
        with contextlib.suppress(OSError):
            os.close(fd)
