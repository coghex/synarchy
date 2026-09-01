#!/usr/bin/env python3
"""One probe, one claimant: the atomic per-probe claim (#1434).

Many `/deflake` agents run at once, one probe each. Two of them
measuring the SAME probe is an hour of duplicated engine time and two
conflicting census records, and nothing in the harness prevented it:
`tools/run_probes.py` coordinates only the probes inside one runner
process, and `tools/probe_flake.py`'s port leases and live-invocation
registry coordinate host-global PORTS and a concurrency figure, not
probe identity.

This module is the claim, and the one claim-aware orchestration
boundary that holds it:

    python3 tools/probe_claim.py --probe role --runs 10 --result /tmp/r.json
    python3 tools/probe_claim.py --status

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
the claim. It serializes the read-decide-write of the claim file, which
is what makes TAKEOVER single-successor: reclaiming a lapsed claim is
unavoidably a read-then-write, and two reclaimers doing that unserialized
is the classic race where the second one's write lands on top of the
first one's fresh claim. Held for microseconds per operation, never
across a measurement — so a crashed holder releases the sidecar
instantly and still holds its claim until the lease runs out.

Identity and namespace
----------------------
Claims are keyed by the canonical `probe_runner_registry.PROBES` key and nothing
else, so the two spellings of a probe cannot claim it twice. The
namespace is `<git-common-dir>/probe-claims`, resolved from the
REPOSITORY-common git directory: every linked worktree of one repository
resolves the same directory, and it moves with none of the three things
that would split the namespace and let two agents both believe they own
a probe — the current worktree, `--artifact-root`, and `TMPDIR`. It is
repository-scoped rather than host-global on purpose; a port is a host
resource, a probe key is a repository's. Untracked per-repository
coordination state under the common git directory is the established
convention here, not a new one: `tools/probe_inflight.py` reads the
`$test` coordinator's `<git-common-dir>/codex-test` tree the same way.

The lease, and why it is renewed rather than sized
--------------------------------------------------
No fixed TTL can be both safe and correct here: `probe_flake.measure`
accepts any positive `--runs` and each run inherits a 900-second
timeout, so a supported ten-run measurement legitimately outlives any
constant. A live claim is therefore RENEWED — a background renewer
refreshes the lease while the measurement runs, and `LEASE_SECONDS` only
has to exceed one run's worst case, which it does with a wide margin. A
long-running supported measurement never becomes reclaimable; a dead
holder's claim lapses one lease after its last renewal.

Two things make that real rather than nominal. The orchestration
boundary REFUSES a lease shorter than
`MIN_ORCHESTRATION_LEASE_SECONDS`, twice one run's timeout, because a
lease that can elapse while a single run is still going is not a short
lease but a broken one — it hands the probe to a second agent
mid-measurement. And claim timestamps carry MICROSECONDS, because
whole-second stamps round a lease down: a sub-second lease would be
born already expired and any lease could lapse a second early. Neither
is defence in depth for the other; each closes a different half.

Ownership safety
----------------
Every acquisition mints a unique `token`, and release, renewal and
takeover are all checked against it. Two concurrent reclaimers of one
lapsed claim yield exactly one successor. An expired owner that wakes up
late and exits cleanly finds a token that is not its own and leaves the
successor's claim alone rather than deleting it — the failure mode that
would hand one probe to two agents at once.

Every instant a decision is judged against is read INSIDE the sidecar
lock, never before it. Waiting for that lock takes as long as the writer
ahead of us and can exceed a lease outright, so a pre-sampled instant is
wrong in both directions at once: a claim written from it is stamped
already expired, and a claim denied against one may have lapsed during
the wait.

EXPIRY IS ONE-WAY, and the token alone does not undo it. A process that
stalled past its own lease — suspended, swapped out, stopped in a
debugger — has a renewer that wakes up eventually, and renewing there
would revive a claim that had already lapsed. Nobody need have taken
the probe for that to be wrong: it denies a claimant entitled to
reclaim it, and it makes the lease mean nothing whenever the holder is
merely slow rather than dead. A lapsed claim is acquired again, by
whoever gets there first, or not at all.

A claim file that is empty, truncated, unparseable or INCOMPLETE is
treated as OCCUPIED until its own filesystem age reaches the lease, then
becomes reclaimable. That covers a crash between the exclusive create
and the payload write without opening a window in which a competing
acquisition succeeds immediately. Completeness is checked against every
field a claim carries, not merely the ones an ownership decision reads:
a partial file holding nothing but a probe, a token and a far-future
expiry would otherwise read as a live claim forever — never aged out,
because ageing out is what happens to malformed claims — and one stray
write would wedge the probe permanently.

CONSISTENCY is checked alongside completeness, because well-typed
fields can still contradict each other. A one-second `lease_seconds`
beside an `expires_at` years away is a file that never expires and
never ages out either; so the lease has to be the distance between the
two timestamps that describe it, within exactly the rounding a legacy
second-precision file carries.

The orchestration boundary
--------------------------
`run_claimed_measurement` is where the claim is worth having. In order:

1. reject an unmeasurable probe before claiming anything;
2. acquire the claim — a DENIED claimant stops here, having created no
   artifact directory, no result document and no census entry, and
   reports the current owner and the claim's age;
3. record the acquisition in the census, BEFORE the probe runs, inside
   one hold of the sidecar lock that renews the lease first — that
   write is a census mutation and can block on another writer for as
   long as that writer takes, so it gets the same protection the
   ingestion does. If it fails, or no `docs-wip` census is reachable,
   the claim is released and the measurement is refused with a
   controlled diagnostic — a measurement nobody can attribute is worse
   than one that did not happen;
4. reassert ownership immediately before the probe starts, and REFUSE
   TO START if it is gone. Anything may have happened since the audit —
   a suspended machine, a census write that blocked for an hour — and
   beginning a measurement this run no longer owns is exactly the
   duplicated work the claim exists to prevent;
5. run the measurement, renewing the lease throughout;
6. RETAIN the completed measurement on disk — the caller's `--result`
   path, or the run's own invocation directory — before anything that
   can fail touches it. The measurement is the expensive thing here and
   everything after it is cheap, so no census refusal, lost claim or
   missing docs worktree may cost an hour of engine time. The retained
   file is a complete `probe-flake-result/v1`, so
   `probe_census.py --record` ingests it once the cause is fixed;
7. ingest the result — success or harness error — inside ONE hold of
   the sidecar lock that first re-reads the claim file and confirms the
   claim is still ours and still live, and renews the lease so it
   cannot elapse mid-commit. Checking and then writing would be two
   steps with a gap between them: the renewer has stopped by then, so a
   slow census commit could outlive the lease, another agent could
   acquire the probe and start measuring, and this process would
   publish anyway on a stale answer. Under the hold no acquisition can
   interleave, because every acquisition takes that same lock. If the
   claim was ALREADY lost, a second agent may have been measuring the
   same probe, so neither result is the exclusive observation the
   census records: nothing is ingested, the artifacts are kept, and the
   run reports the loss. The renewer's `lost` flag is a hint, not the
   authority — it sees only what a renewal happened to hit;
8. release, checked against this acquisition's own token.

The lock ORDER is claim-then-census everywhere, so the two never wait
on each other.

`tools/probe_flake.py` is deliberately NOT changed by any of this. Its
own contract is that the harness behaves identically on a checkout with
no docs worktree, so the census-backed claim lives here, in the
orchestration path, and the low-level measurement API stays usable on
its own.

Exit codes:
  0  the measurement ran and was ingested (whatever rate it observed)
  2  rejected before anything was claimed or created
  3  ALREADY CLAIMED: another agent holds this probe; nothing was
     created and nothing was recorded
  4  harness error: the measurement's protocol stream could not be
     trusted. The non-accepted attempt is still ingested
  5  the census could not durably record this run. BEFORE the probe
     ran that is a claim audit failure: the acquisition was not
     recordable, so the measurement was refused and the claim released
     with nothing created. AFTER it ran the measurement really
     happened, so it is retained on disk instead — the diagnostic names
     the file and the `--record` command that ingests it, and
     re-running the probe is never the recovery
  6  no clear, leasable port in the whole range
  7  the claim was lost. Before the probe started, the probe was NOT
     run, because starting a measurement this run no longer owns is the
     duplicated work the claim exists to prevent. After it ran, another
     agent may have been measuring the probe too, so NOTHING is
     ingested — an unattributable measurement is not a measurement —
     while the artifacts and the retained result document are kept
"""
from __future__ import annotations

import argparse
import contextlib
import errno
import fcntl
import json
import math
import os
import socket
import stat
import subprocess
import sys
import tempfile
import threading
import time
import uuid
from datetime import datetime, timedelta, timezone
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_census  # noqa: E402
import probe_flake  # noqa: E402
import probe_engine  # noqa: E402
import probe_runner_registry  # noqa: E402

CLAIM_SCHEMA = "probe-claim/v1"

# The claim namespace, under the repository-common git directory.
CLAIM_DIR_NAME = "probe-claims"
LOCK_PREFIX = "."
LOCK_SUFFIX = ".lock"
STAGING_PREFIX = ".probe_claim."
STAGING_SUFFIX = ".tmp"

# One run's worst case is `probe_runner_registry.DEFAULT_TIMEOUT` (900 s). The lease
# only has to outlive ONE run, because the renewer refreshes it between
# runs and on its own clock; the wide margin is what keeps a stalled
# renewer thread from dropping a live claim.
LEASE_SECONDS = 4.0 * probe_runner_registry.DEFAULT_TIMEOUT
# The floor `run_claimed_measurement` and the CLI enforce. A lease that
# can elapse while ONE supported run is still going is not a short
# lease, it is a broken one: the probe becomes reclaimable mid-
# measurement and two agents end up measuring it at once. Twice the
# per-run timeout is the smallest value that cannot do that, and it is
# refused rather than silently raised, because a caller asking for
# thirty seconds has misunderstood what this lease is for.
MIN_ORCHESTRATION_LEASE_SECONDS = 2.0 * probe_runner_registry.DEFAULT_TIMEOUT
# Renew this often. Three refreshes per lease means two consecutive
# missed renewals still leave the claim held. The lower clamp is a
# guard against a zero interval, NOT a floor in seconds: a floor above
# a short lease would schedule the first renewal after the claim had
# already lapsed, which is precisely the failure it looks like it
# prevents. `acquire` still accepts any positive lease, so the
# self-test can drive expiry deliberately; only the orchestration
# boundary above enforces a usable one.
RENEW_DIVISOR = 3
MIN_RENEW_INTERVAL = 0.05
# The upper bound, and it is not arbitrary. `timedelta` overflows well
# below the float range — `timedelta(seconds=1e100)` raises — and the
# census `claim` definition caps `lease_seconds` at 1e9, so a lease
# above that would be refused when the acquisition was RECORDED, after
# the probe had already been claimed. One billion seconds is thirty-one
# years, which is not a short leash; anything past it is a typo or a
# unit mistake, and both are better refused before anything is claimed.
MAX_LEASE_SECONDS = 1_000_000_000.0

# Where a completed measurement is kept when the caller named no
# `--result` path of its own: beside the run's own artifacts, under the
# invocation directory `probe_flake` already created for it.
RETAINED_RESULT_NAME = "probe-flake-result.json"

# How far a claim's own timestamps may disagree with its lease before it
# is treated as malformed. Not slack for sloppy writers: it is exactly
# the rounding a SECOND-precision file carries, since `renewed_at` and
# `expires_at` were each truncated independently before this module
# started writing microseconds. Anything past it is a file whose fields
# contradict each other.
STAMP_TOLERANCE_SECONDS = 2.0

EXIT_OK = 0
EXIT_REJECTED = 2
EXIT_ALREADY_CLAIMED = 3
EXIT_HARNESS_ERROR = 4
EXIT_CLAIM_AUDIT = 5
EXIT_NO_PORT = 6
EXIT_CLAIM_LOST = 7

CLAIM_FILE_MODE = 0o600
CLAIM_DIR_MODE = 0o700


class ClaimError(Exception):
    """A controlled refusal: nothing was claimed, nothing was created."""


class ClaimDenied(Exception):
    """Another agent holds this probe. The distinct already-claimed outcome.

    Carries whatever the current claim could be read to say — owner,
    host, pid, and the claim's age — so a selector can report WHY a
    candidate was skipped instead of just that it was. A claim that
    could not be parsed still denies, with `owner` None and the age
    measured from the file itself.
    """

    def __init__(self, probe: str, *, owner=None, host=None, pid=None,
                 token=None, age_seconds=None, expires_in_seconds=None,
                 reason: str = ""):
        self.probe = probe
        self.owner = owner
        self.host = host
        self.pid = pid
        self.token = token
        self.age_seconds = age_seconds
        self.expires_in_seconds = expires_in_seconds
        self.reason = reason
        super().__init__(self.describe())

    def describe(self) -> str:
        parts = [f"probe {self.probe!r} is already claimed"]
        if self.owner:
            parts.append(f"by {self.owner}")
        if self.age_seconds is not None:
            detail = f"{self.age_seconds:.0f}s ago"
            if self.expires_in_seconds is not None:
                detail += f", {self.expires_in_seconds:.0f}s of lease left"
            parts.append(f"({detail})")
        if self.reason:
            parts.append(f"— {self.reason}")
        return " ".join(parts)

    def to_document(self) -> dict:
        return {
            "outcome": "already-claimed",
            "probe": self.probe,
            "owner": self.owner,
            "host": self.host,
            "pid": self.pid,
            "token": self.token,
            "age_seconds": self.age_seconds,
            "expires_in_seconds": self.expires_in_seconds,
            "reason": self.reason or None,
        }


class ClaimLost(Exception):
    """The claim we held is gone or belongs to someone else now."""


class ClaimAuditFailed(Exception):
    """The acquisition could not be durably recorded, so nothing ran."""


# The two phases at which a run can discover it no longer owns its probe.
PHASE_BEFORE_MEASUREMENT = "before the measurement started"
PHASE_AFTER_MEASUREMENT = "while the probe was running"


class ResultIngestionFailed(Exception):
    """The measurement completed, but the census would not take it.

    Distinct from `ClaimAuditFailed`, which happens BEFORE the probe
    runs and costs nothing: by here an hour of engine time exists, so
    the failure carries the path the completed measurement was retained
    at. Recovery is re-ingesting that file, never re-running the probe.
    """

    def __init__(self, message: str, *, measurement=None, retained=None):
        super().__init__(message)
        self.measurement = measurement
        self.retained = retained


class ClaimLostDuringRun(Exception):
    """This run stopped being the probe's owner partway through.

    Two phases reach it, and the difference is what has already
    happened, not what the run does about it:

    * BEFORE the measurement — the acquisition audit outlived the lease
      and somebody took the probe. Nothing runs, because starting now
      would be the duplicate measurement this whole feature exists to
      prevent.
    * AFTER it — the measurement really happened, but a second agent
      may have been measuring the same probe at the same time, so the
      result is not attributable to one exclusive run and is not
      ingested.
    """

    def __init__(self, message: str, *, phase: str, measurement=None):
        super().__init__(message)
        self.phase = phase
        self.measurement = measurement


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


def _ensure_root(root: Path) -> Path:
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


def require_probe_key(probe) -> str:
    """The canonical `probe_runner_registry.PROBES` key, or a controlled refusal.

    Claims are keyed by the canonical key and only by it, so the several
    human spellings of one probe cannot claim it twice. Validating
    against the registry also means no claim path is ever built from an
    unvetted string.
    """
    if not isinstance(probe, str) or not probe:
        raise ClaimError(f"a probe key must be a non-empty string, got {probe!r}")
    known = {key for key, _script, _purpose in probe_runner_registry.PROBES}
    if probe not in known:
        raise ClaimError(
            f"unknown probe {probe!r}: not registered in probe_runner_registry.PROBES. "
            f"`python3 tools/run_probes.py --list` names every probe.")
    return probe


def require_lease(value, what: str = "a claim lease") -> float:
    """A finite, positive number of seconds, or a controlled refusal.

    FINITENESS is the part that is easy to leave out and impossible to
    recover from. `float` parses `nan`, `inf` and `-inf` — so
    `--lease-seconds nan` is a perfectly valid argument — and NaN fails
    every ordering comparison, so a bare `value <= 0` or
    `value < FLOOR` waves both NaN and positive infinity straight
    through. They then reach `timedelta`, which raises `ValueError` and
    `OverflowError` respectively: a traceback out of an acquisition,
    where this module promises a controlled refusal.

    The upper bound is the same kind of trap one step along: a lease is
    finite and positive and still unusable if it is large enough that
    `timedelta` cannot represent it, which raises `OverflowError` from
    inside `_payload` — again a traceback where a refusal was promised.
    `MAX_LEASE_SECONDS` is where the census's own cap sits, so a lease
    this accepts is one the acquisition record can also hold.

    `True` is excluded explicitly, because `isinstance(True, int)`.
    """
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ClaimError(
            f"{what} must be a number of seconds, got {value!r}")
    if not math.isfinite(value):
        raise ClaimError(
            f"{what} must be a finite number of seconds, got {value!r}")
    if value <= 0:
        raise ClaimError(
            f"{what} must be a positive number of seconds, got {value!r}")
    if value > MAX_LEASE_SECONDS:
        raise ClaimError(
            f"{what} must be at most {MAX_LEASE_SECONDS:.0f} seconds "
            f"(about thirty-one years), got {value!r}")
    return float(value)


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


def _owner_description() -> str:
    user = os.environ.get("USER") or os.environ.get("LOGNAME") or f"uid{os.getuid()}"
    return f"{user}@{socket.gethostname()}:{os.getpid()}"


def _payload(probe: str, token: str, *, acquired: datetime, renewed: datetime,
             lease_seconds: float, owner: str, worktree: str) -> dict:
    return {
        "schema": CLAIM_SCHEMA,
        "probe": probe,
        "token": token,
        "owner": owner,
        "host": socket.gethostname(),
        "pid": os.getpid(),
        "worktree": worktree,
        "acquired_at": stamp(acquired),
        "renewed_at": stamp(renewed),
        "expires_at": stamp(renewed + timedelta(seconds=lease_seconds)),
        "lease_seconds": lease_seconds,
    }


def _encode(payload: dict) -> bytes:
    return (json.dumps(payload, indent=2, sort_keys=True) + "\n").encode("utf-8")


def _read_payload(path: Path, expected: str | None = None):
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
    field `_payload` writes is therefore required here, so a partial
    write is recognized as partial whatever it happens to contain.

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


def _install(path: Path, payload: dict) -> None:
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


def _create_exclusive(path: Path, payload: dict) -> bool:
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
def _serialized(probe: str, root: Path):
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


class Claim:
    """A held claim on one probe. The token is what makes it ours."""

    def __init__(self, probe: str, token: str, path: Path, root: Path,
                 payload: dict, lease_seconds: float):
        self.probe = probe
        self.token = token
        self.path = path
        self.root = root
        self.payload = payload
        self.lease_seconds = lease_seconds
        self.released = False

    # -- context management -------------------------------------------------
    def __enter__(self) -> "Claim":
        return self

    def __exit__(self, exc_type, exc, tb) -> bool:
        # Released on EVERY managed exit — a normal return, a raised
        # exception, a KeyboardInterrupt, a SystemExit. What cannot run
        # here is a SIGKILL or a hard crash, and that is exactly the case
        # the lease exists to recover.
        #
        # A release that itself fails while another exception is already
        # unwinding is suppressed: the caller needs the error that got
        # them here, and the claim lapses on its own lease either way.
        try:
            self.release()
        except ClaimError:
            if exc_type is None:
                raise
        return False

    # -- operations ---------------------------------------------------------
    def _renew_locked(self, moment: datetime) -> dict:
        """`renew`'s body. The sidecar lock must already be held.

        An EXPIRED claim is not renewable, even when the token still
        matches and nobody has taken it yet. A process that stalled past
        its own lease — suspended, swapped out, stopped in a debugger —
        has a renewer that wakes up eventually, and letting it revive a
        lapsed claim would deny a claimant entitled to reclaim the probe
        and would make the lease mean nothing whenever the holder is
        slow rather than dead. Expiry is one-way: the claim must be
        acquired again, by whoever gets there first.
        """
        document, _mtime = _read_payload(self.path, self.probe)
        if document is None or document.get("token") != self.token:
            raise ClaimLost(
                f"the claim on {self.probe!r} is no longer ours "
                f"(token {self.token!r} is not the one on disk)")
        expires = parse_stamp(document.get("expires_at"))
        if expires is None or moment >= expires:
            raise ClaimLost(
                f"the claim on {self.probe!r} expired at "
                f"{document.get('expires_at')!r} and cannot be renewed; it "
                f"has to be acquired again")
        payload = dict(document)
        payload["renewed_at"] = stamp(moment)
        payload["expires_at"] = stamp(
            moment + timedelta(seconds=self.lease_seconds))
        payload["lease_seconds"] = self.lease_seconds
        _install(self.path, payload)
        self.payload = payload
        return payload

    def _holds_locked(self, moment: datetime) -> bool:
        """`holds`'s body. The sidecar lock must already be held."""
        document, _mtime = _read_payload(self.path, self.probe)
        if document is None or document.get("token") != self.token:
            return False
        expires = parse_stamp(document.get("expires_at"))
        return expires is not None and moment < expires

    def renew(self, now: datetime | None = None) -> dict:
        """Extend this claim's lease. Token-checked.

        Raises `ClaimLost` when the claim is gone or now carries another
        token: an expired owner must never write over its successor.
        """
        with _serialized(self.probe, self.root):
            return self._renew_locked(now if now is not None else utc_now())

    def release(self) -> bool:
        """Give the claim back. Token-checked; True when we removed it.

        A claim carrying someone ELSE's token is left exactly where it
        is. That is the late-release case: our lease lapsed, a successor
        took the probe, and this process is only now unwinding. Deleting
        that file would hand one probe to two agents at once.
        """
        if self.released:
            return False
        with _serialized(self.probe, self.root):
            document, _mtime = _read_payload(self.path, self.probe)
            self.released = True
            if document is None or document.get("token") != self.token:
                return False
            try:
                self.path.unlink()
            except FileNotFoundError:
                return False
            except OSError as error:
                raise ClaimError(
                    f"could not release the claim {self.path} "
                    f"({error})") from None
            return True

    def holds(self, now: datetime | None = None) -> bool:
        """Is this claim still ours, and still live, RIGHT NOW?

        Read from disk rather than from `self.payload`, because the
        question is exactly whether something else changed it. False
        means the claim lapsed and somebody took it over, or it was
        removed — either way this process no longer speaks for the
        probe, and anything it was about to publish is not attributable.

        A bare `holds()` is a SNAPSHOT and answers only about the
        instant it read. Anything whose correctness depends on the
        answer still being true while it runs must use
        `commit_while_held` instead.
        """
        with _serialized(self.probe, self.root):
            return self._holds_locked(now if now is not None else utc_now())

    def commit_while_held(self, commit, now: datetime | None = None):
        """Run `commit()` while this claim is provably, exclusively ours.

        Checking ownership and then acting on the answer are two steps,
        and between them the lease can lapse and another agent can take
        the probe — so the check alone is not enough for anything that
        PUBLISHES. This closes the gap by doing both inside ONE hold of
        the sidecar lock: no acquisition can interleave, because every
        acquisition path takes that same lock.

        The lease is renewed inside the hold as well, so it cannot
        elapse while `commit` runs, however long the commit takes.

        `commit` must not touch this claim itself — it would deadlock on
        the lock this already holds. The census is the intended callee,
        and the lock ORDER is claim-then-census everywhere, so the two
        never wait on each other.
        """
        with _serialized(self.probe, self.root):
            # Read INSIDE the hold: waiting for the lock can take as
            # long as the writer ahead of us, and an instant sampled
            # before that wait is stale in the permissive direction.
            moment = now if now is not None else utc_now()
            if not self._holds_locked(moment):
                raise ClaimLost(
                    f"the claim on {self.probe!r} is no longer ours "
                    f"(token {self.token!r} is not the live one on disk)")
            self._renew_locked(moment)
            return commit()

    def reassert(self, now: datetime | None = None) -> None:
        """Confirm the claim is still ours and live, and refresh its lease.

        `commit_while_held` with nothing to commit: the check and the
        renewal are the point. Used at a boundary where the next step
        must not begin unless this run still owns the probe.
        """
        self.commit_while_held(lambda: None, now=now)

    def census_record(self, *, commit_sha: str, requested_runs: int) -> dict:
        """This acquisition, in the census's `claim` shape."""
        acquired = parse_stamp(self.payload["acquired_at"]) or utc_now()
        return {
            "token": self.token,
            "timestamp_utc": stamp_second(acquired),
            "commit_sha": commit_sha,
            "owner": self.payload["owner"],
            "host": self.payload["host"],
            "pid": self.payload["pid"],
            "lease_seconds": self.lease_seconds,
            "requested_runs": requested_runs,
        }


def acquire(probe: str, *, root: Path | None = None,
            lease_seconds: float = LEASE_SECONDS,
            now: datetime | None = None,
            repo_root: str | None = None) -> Claim:
    """Take the claim on `probe`, or raise `ClaimDenied`.

    Three outcomes, decided under the sidecar lock so that concurrent
    callers see one another's decisions:

    * NO CLAIM — `O_CREAT|O_EXCL` it into existence and win;
    * A LIVE CLAIM — deny, reporting its owner and age;
    * A LAPSED OR MALFORMED-AND-AGED CLAIM — take it over by atomic
      replacement and win. Exactly one of several concurrent reclaimers
      can be here at a time, so exactly one succeeds and the rest find a
      live claim and deny.

    `lease_seconds` is the TTL in force for THIS acquisition: it is how
    long the new claim lives before a renewal, and it is also the age at
    which an unparseable claim stops being treated as occupied.
    """
    key = require_probe_key(probe)
    lease_seconds = require_lease(lease_seconds)
    base = _ensure_root(Path(root) if root is not None
                        else repository_claim_root(repo_root))
    path = claim_path(key, base)
    token = uuid.uuid4().hex

    with _serialized(key, base):
        # The clock is read INSIDE the hold, and the payload is built
        # from it here rather than above. Waiting for this lock takes as
        # long as the writer ahead of us, which can exceed the lease
        # outright — and an instant sampled before that wait is wrong in
        # both directions at once: the claim we go on to write would be
        # stamped with an `expires_at` already in the past, and a claim
        # we go on to DENY against might have expired while we waited.
        # `now` stays injectable so the self-test can pose a specific
        # instant; nothing else may pre-sample one.
        moment = now if now is not None else utc_now()
        payload = _payload(key, token, acquired=moment, renewed=moment,
                           lease_seconds=float(lease_seconds),
                           owner=_owner_description(),
                           worktree=str(Path(repo_root
                                             or probe_engine.REPO_ROOT)))
        document, mtime = _read_payload(path, key)
        if document is None and mtime is None:
            if _create_exclusive(path, payload):
                return Claim(key, token, path, base, payload,
                             float(lease_seconds))
            # Someone created it between the read and the create, while
            # we held the lock — which should be impossible, so it is
            # reported rather than retried into.
            raise ClaimDenied(
                key, reason="the claim appeared during acquisition")
        if document is None:
            # Empty, truncated or unparseable: occupied until its own
            # filesystem age reaches the lease. A crash between the
            # exclusive create and the payload write lands here, and must
            # not let a competitor in immediately.
            age = max(0.0, moment.timestamp() - (mtime or 0.0))
            if age < float(lease_seconds):
                raise ClaimDenied(
                    key, age_seconds=age,
                    reason=f"its claim file is unreadable and is only "
                           f"{age:.0f}s old, so it is treated as held until "
                           f"the {float(lease_seconds):.0f}s lease elapses")
            _install(path, payload)
            return Claim(key, token, path, base, payload, float(lease_seconds))
        expires = parse_stamp(document.get("expires_at"))
        if expires is not None and moment < expires:
            acquired = parse_stamp(document.get("acquired_at"))
            raise ClaimDenied(
                key,
                owner=document.get("owner"),
                host=document.get("host"),
                pid=document.get("pid"),
                token=document.get("token"),
                age_seconds=((moment - acquired).total_seconds()
                             if acquired is not None else None),
                expires_in_seconds=(expires - moment).total_seconds())
        _install(path, payload)
        return Claim(key, token, path, base, payload, float(lease_seconds))


def read_claim(probe: str, *, root: Path | None = None,
               repo_root: str | None = None):
    """The current claim on `probe` as stored, or None. Read-only."""
    key = require_probe_key(probe)
    base = Path(root) if root is not None else repository_claim_root(repo_root)
    if not base.is_dir():
        return None
    document, _mtime = _read_payload(claim_path(key, base), key)
    return document


class Renewer:
    """Keeps a live claim alive while a long measurement runs.

    A daemon thread rather than a hook on the measurement: a single run
    can occupy the full 900-second timeout, so renewal must not depend on
    the measurement handing control back. `lost` records a claim that was
    taken from us, which the orchestration reports rather than swallowing
    — it means two agents may have measured one probe.
    """

    def __init__(self, claim: Claim, interval: float | None = None):
        self.claim = claim
        # A non-finite interval would make `Event.wait` return at once
        # and spin, so it is refused here rather than tolerated.
        self.interval = (require_lease(interval, "a renewal interval")
                         if interval is not None
                         else max(MIN_RENEW_INTERVAL,
                                  claim.lease_seconds / RENEW_DIVISOR))
        self.lost: str | None = None
        self.renewals = 0
        self._stop = threading.Event()
        self._thread: threading.Thread | None = None

    def _loop(self) -> None:
        while not self._stop.wait(self.interval):
            try:
                self.claim.renew()
                self.renewals += 1
            except ClaimLost as error:
                self.lost = str(error)
                return
            except ClaimError as error:
                self.lost = str(error)
                return

    def __enter__(self) -> "Renewer":
        self._thread = threading.Thread(
            target=self._loop, name=f"probe-claim-renew-{self.claim.probe}",
            daemon=True)
        self._thread.start()
        return self

    def __exit__(self, exc_type, exc, tb) -> bool:
        self._stop.set()
        if self._thread is not None:
            self._thread.join(timeout=30)
        return False


# --------------------------------------------------------------------------
# The orchestration boundary
# --------------------------------------------------------------------------
def check_result_path(result_path):
    """A writable `--result` destination, or a refusal, BEFORE anything runs.

    Checked in front of the claim rather than after the measurement, for
    the reason everything else in this module is: an operator who asked
    for a result document and mistyped the directory should find that
    out in the first second, not an hour later holding a measurement
    that has nowhere to go.
    """
    if result_path is None:
        return None
    target = Path(result_path)
    # The TARGET, not merely its directory. An existing directory at
    # that path, an existing file nobody may write, a broken symlink —
    # each has a perfectly writable parent and each fails only when the
    # measurement is already an hour old.
    if os.path.lexists(target):
        try:
            info = os.stat(target)
        except OSError as error:
            raise ClaimError(
                f"the result document cannot be written to {target} "
                f"({error}); a dangling symlink is not a destination") from None
        if stat.S_ISDIR(info.st_mode):
            raise ClaimError(
                f"the result document cannot be written to {target}: it is a "
                f"directory")
        if not stat.S_ISREG(info.st_mode):
            raise ClaimError(
                f"the result document cannot be written to {target}: it must "
                f"be a regular file (got mode {stat.S_IFMT(info.st_mode):#o})")
        if not os.access(target, os.W_OK):
            raise ClaimError(
                f"the result document cannot be written to {target}: it is "
                f"not writable")
        return target
    try:
        target.parent.mkdir(parents=True, exist_ok=True)
    except OSError as error:
        raise ClaimError(
            f"the result document cannot be written to {target}: its "
            f"directory could not be created ({error})") from None
    if not target.parent.is_dir():
        raise ClaimError(
            f"the result document cannot be written to {target}: "
            f"{target.parent} is not a directory")
    if not os.access(target.parent, os.W_OK | os.X_OK):
        raise ClaimError(
            f"the result document cannot be written to {target}: "
            f"{target.parent} is not writable")
    return target


def retain_measurement(measurement, result_path=None):
    """Put the completed measurement's document somewhere it survives.

    A measurement is the expensive thing here — ten runs of a real
    engine, up to an hour — and everything after it is cheap. So it is
    written to disk the moment it exists, BEFORE the census ingestion
    that could fail and unwind past it. A census that refuses the
    document, a lost claim, an unreachable docs worktree: none of them
    may cost the operator the measurement, because the retained
    document is a complete `probe-flake-result/v1` that
    `probe_census.py --record` can ingest once the cause is fixed.

    The caller's own `--result` path is preferred, and the run's
    invocation directory is the fallback so a measurement is never lost
    merely because nobody asked for a copy. Returns
    `(path_or_None, problem_or_None)`; it raises nothing, because a
    failure to retain must not become the reason a completed
    measurement is discarded.
    """
    targets = []
    if result_path is not None:
        targets.append(Path(result_path))
    invocation = getattr(measurement, "invocation_dir", None)
    if invocation is not None:
        targets.append(Path(invocation) / RETAINED_RESULT_NAME)
    problem = None
    for target in targets:
        try:
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(
                json.dumps(measurement.to_document(), indent=2,
                           sort_keys=True) + "\n",
                encoding="utf-8")
            return target, problem
        except (OSError, TypeError, ValueError) as error:
            if problem is None:
                problem = f"could not write {target} ({error})"
    return None, problem


class Outcome:
    """What one claim-aware measurement did, and what it wrote."""

    def __init__(self, *, outcome: str, probe: str, exit_code: int,
                 claim=None, measurement=None, denied=None, detail: str = "",
                 census_path=None, claim_lost=None, result_path=None,
                 result_problem=None):
        self.outcome = outcome
        self.probe = probe
        self.exit_code = exit_code
        self.claim = claim
        self.measurement = measurement
        self.denied = denied
        self.detail = detail
        self.census_path = census_path
        self.claim_lost = claim_lost
        self.result_path = result_path
        self.result_problem = result_problem

    def to_document(self) -> dict:
        if self.denied is not None:
            return self.denied.to_document()
        document = {
            "outcome": self.outcome,
            "probe": self.probe,
            "token": self.claim.token if self.claim is not None else None,
            "census": str(self.census_path) if self.census_path else None,
            "detail": self.detail or None,
            "claim_lost": self.claim_lost,
            "result_document": (str(self.result_path)
                                if self.result_path else None),
            "result_problem": self.result_problem,
        }
        if self.measurement is not None:
            document["status"] = self.measurement.status
            document["completed_runs"] = len(self.measurement.runs)
            document["requested_runs"] = self.measurement.requested_runs
            document["failure_rate"] = self.measurement.failure_rate
        return document


def run_claimed_measurement(probe: str, runs: int, *,
                            artifact_root: Path | None = None,
                            rts_caps: int = probe_flake.DEFAULT_RTS_CAPS,
                            lease_seconds: float = LEASE_SECONDS,
                            announce=None,
                            root: Path | None = None,
                            repo_root: str | None = None,
                            census_path: Path | None = None,
                            result_path=None,
                            measure=None, record_claim=None, record_result=None,
                            renew_interval: float | None = None) -> Outcome:
    """Claim `probe`, measure it, ingest the result, release. In that order.

    The claim is held from BEFORE any artifact directory or probe
    process exists through the durable ingestion of the resulting
    record, success or harness error alike. Every early exit leaves the
    census exactly as it found it:

    * an unmeasurable probe is rejected before a claim is even attempted;
    * a denied claimant creates no artifacts, writes no result document
      and records nothing;
    * an acquisition that cannot be durably recorded releases the claim
      and refuses to run the probe at all;
    * a measurement that finishes without the claim still being ours
      ingests NOTHING, because a probe two agents may have been running
      at once has no attributable result.

    A COMPLETED measurement, however, is never thrown away. It is
    written to disk — `result_path` when one is given, the run's own
    invocation directory otherwise — before the ingestion that might
    fail, so no census refusal, lost claim or missing docs worktree can
    cost an hour of engine time. The retained file is a complete
    `probe-flake-result/v1`, so `probe_census.py --record` ingests it
    once the cause is fixed, and every diagnostic on those paths names
    where it is.

    `lease_seconds` is floored at `MIN_ORCHESTRATION_LEASE_SECONDS`
    here — twice one run's timeout — and refused below it. That is what
    stops the lease elapsing mid-measurement in the first place; the
    ownership check before ingestion is the backstop for everything a
    floor cannot foresee, such as a machine suspended for an hour.
    """
    key = require_probe_key(probe)
    # Before claiming: a probe this harness cannot measure at all is not
    # a probe worth taking off another agent's candidate list.
    probe_flake.resolve_probe(key)
    if runs < 1:
        raise ClaimError(f"--runs must be a positive count, got {runs}")
    # Type, finiteness and positivity first, so a `nan` or an `inf`
    # meets the refusal that names what is wrong with it rather than the
    # floor message — and, more to the point, so neither reaches an
    # ordering comparison that would wave it through.
    lease_seconds = require_lease(lease_seconds, "a claim lease")
    result_target = check_result_path(result_path)
    if lease_seconds < MIN_ORCHESTRATION_LEASE_SECONDS:
        raise ClaimError(
            f"a claim lease of {lease_seconds!r} cannot survive one run of "
            f"{probe!r}: a single run may take the full "
            f"{probe_runner_registry.DEFAULT_TIMEOUT:.0f}s timeout, so a lease under "
            f"{MIN_ORCHESTRATION_LEASE_SECONDS:.0f}s would let another agent "
            f"reclaim this probe while it is still being measured. Raise "
            f"--lease-seconds to at least "
            f"{MIN_ORCHESTRATION_LEASE_SECONDS:.0f}.")

    # The three seams #1436 and the gate drive this through. Each
    # defaults to the real thing, so the shipped path is the one that
    # runs unless a caller deliberately substitutes.
    run_measure = measure if measure is not None else probe_flake.measure
    log_claim = record_claim or probe_census.record_claim
    log_result = record_result or probe_census.record_result
    try:
        claim = acquire(key, root=root, lease_seconds=lease_seconds,
                        repo_root=repo_root)
    except ClaimDenied as denied:
        return Outcome(outcome="already-claimed", probe=key,
                       exit_code=EXIT_ALREADY_CLAIMED, denied=denied,
                       detail=denied.describe())

    with claim:
        target = (Path(census_path) if census_path is not None
                  else probe_census.manifest_path(repo_root))
        record = claim.census_record(commit_sha=commit_sha(repo_root),
                                     requested_runs=runs)
        # The acquisition audit runs under the SAME hold the ingestion
        # does, for the same reason: it is a census mutation, it can
        # block on another writer's lock for as long as that writer
        # takes, and a lease that elapses meanwhile would let a second
        # agent take the probe. Under the hold no acquisition can
        # interleave, and the lease is renewed before the write begins.
        try:
            claim.commit_while_held(lambda: log_claim(target, key, record))
        except (probe_census.CensusError,
                probe_census.DocsWorktreeMissing) as error:
            # The claim is released by the context manager on the way
            # out. Nothing has run, so nothing is lost by refusing —
            # and a measurement nobody can attribute to an acquisition
            # is worse than a measurement that did not happen.
            raise ClaimAuditFailed(
                f"probe {key!r}: the claim was acquired but could not be "
                f"recorded in the census at {target} ({error}); the claim "
                f"has been released and the probe was not run") from None
        except ClaimLost as error:
            raise ClaimLostDuringRun(
                f"probe {key!r}: the claim was lost while its acquisition "
                f"was being recorded ({error}), so another agent now owns "
                f"this probe; it was not run and nothing was recorded in "
                f"{target}",
                phase=PHASE_BEFORE_MEASUREMENT) from None

        with Renewer(claim, renew_interval) as renewer:
            # The last thing before the probe starts: still ours, still
            # live, lease refreshed. Anything at all may have happened
            # between the audit and here — a suspended machine, a
            # census write that blocked for an hour — and starting a
            # measurement this run no longer owns is precisely the
            # duplicated work the claim exists to prevent. A stale
            # answer is worth nothing, so this is a fresh read.
            try:
                claim.reassert()
            except ClaimLost as error:
                raise ClaimLostDuringRun(
                    f"probe {key!r}: the claim was lost before the "
                    f"measurement started ({error}), so another agent now "
                    f"owns this probe; it was not run and nothing was "
                    f"recorded in {target}",
                    phase=PHASE_BEFORE_MEASUREMENT) from None
            measurement = run_measure(
                key, runs, artifact_root=artifact_root, rts_caps=rts_caps,
                announce=announce)

        # The measurement exists now, and it is the expensive thing.
        # Everything below can fail; none of it may take this with it.
        retained, result_problem = retain_measurement(measurement,
                                                      result_target)
        kept = (f"the completed measurement was retained at {retained}, and "
                f"`python3 tools/probe_census.py --record {retained}` ingests "
                f"it once the cause is fixed"
                if retained is not None else
                f"the completed measurement could NOT be retained "
                f"({result_problem})")

        # Ownership is CHECKED, not assumed, and the check and the write
        # are ONE operation. `renewer.lost` is a hint — it only sees
        # what a renewal happened to hit — so the authority is a fresh
        # token-and-lease read of the claim file, taken under the
        # sidecar lock and HELD across the ingestion. Checking first and
        # writing afterwards would leave exactly the gap this exists to
        # close: a slow census commit outliving the lease, another agent
        # acquiring the probe and starting to measure it, and this
        # process publishing anyway on the strength of a stale answer.
        #
        # If the claim was already lost, another agent may have been
        # measuring the probe at the same time, and neither result is
        # the exclusive observation the census is a record of. Refuse to
        # ingest rather than record an unattributable one; the run's
        # artifacts stay on disk for whoever investigates.
        try:
            claim.commit_while_held(
                lambda: log_result(target, measurement.to_document()))
        except ClaimLost as error:
            raise ClaimLostDuringRun(
                f"probe {key!r}: the claim was lost while the probe was "
                f"running ({renewer.lost or error}), so another agent may "
                f"have been measuring it at the same time; nothing was "
                f"recorded in {target} and the run's artifacts under "
                f"{measurement.invocation_dir} were kept — {kept}",
                phase=PHASE_AFTER_MEASUREMENT,
                measurement=measurement) from None
        except (probe_census.CensusError,
                probe_census.DocsWorktreeMissing) as error:
            # The census refused a measurement that really happened. The
            # run is NOT repeated to recover from that: it is retained
            # above, and re-ingested by hand once the census is fixed.
            raise ResultIngestionFailed(
                f"probe {key!r}: the measurement completed but could not be "
                f"recorded in the census at {target} ({error}) — {kept}",
                measurement=measurement, retained=retained) from None

    return Outcome(
        outcome="measured" if measurement.valid else "harness-error",
        probe=key,
        exit_code=EXIT_OK if measurement.valid else EXIT_HARNESS_ERROR,
        claim=claim, measurement=measurement, census_path=target,
        claim_lost=renewer.lost, result_path=retained,
        result_problem=result_problem)


# --------------------------------------------------------------------------
# CLI
# --------------------------------------------------------------------------
def _status_rows(root: Path) -> list[dict]:
    rows = []
    if not root.is_dir():
        return rows
    now = utc_now()
    for key, _script, _purpose in probe_runner_registry.PROBES:
        path = claim_path(key, root)
        try:
            document, mtime = _read_payload(path, key)
        except ClaimError as error:
            rows.append({"probe": key, "state": "unusable",
                         "detail": str(error)})
            continue
        if document is None and mtime is None:
            continue
        if document is None:
            rows.append({"probe": key, "state": "malformed",
                         "age_seconds": max(0.0,
                                            now.timestamp() - (mtime or 0.0))})
            continue
        expires = parse_stamp(document.get("expires_at"))
        rows.append({
            "probe": key,
            "state": ("held" if expires is not None and now < expires
                      else "lapsed"),
            "owner": document.get("owner"),
            "token": document.get("token"),
            "acquired_at": document.get("acquired_at"),
            "expires_at": document.get("expires_at"),
        })
    return rows


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--probe", default=None,
                    help="exactly one registered, probe-result/v1 probe key")
    ap.add_argument("--runs", type=int, default=None,
                    help="how many times to run it (positive)")
    ap.add_argument("--status", action="store_true",
                    help="report every claim in this repository and exit")
    ap.add_argument("--result", default=None,
                    help="write the probe-flake-result/v1 document here, as "
                         "soon as the measurement completes and before it is "
                         "ingested, so an ingestion failure cannot cost the "
                         "run")
    ap.add_argument("--artifact-root", default=None,
                    help="override probe_flake's artifact root")
    ap.add_argument("--rts-caps", type=int, default=probe_flake.DEFAULT_RTS_CAPS,
                    help=f"RTS capabilities for every engine "
                         f"(default {probe_flake.DEFAULT_RTS_CAPS})")
    ap.add_argument("--lease-seconds", type=float, default=LEASE_SECONDS,
                    help=f"claim lease before renewal (default "
                         f"{LEASE_SECONDS:.0f}, minimum "
                         f"{MIN_ORCHESTRATION_LEASE_SECONDS:.0f}); the "
                         f"renewer refreshes it while the probe runs")
    ap.add_argument("--json", action="store_true",
                    help="machine-readable outcome on stdout")
    args = ap.parse_args(argv)

    try:
        if args.status:
            rows = _status_rows(repository_claim_root())
            if args.json:
                print(json.dumps({"claims": rows}, indent=2, sort_keys=True))
            elif not rows:
                print("no probe is claimed in this repository")
            else:
                for row in rows:
                    print(f"  {row['probe']:<32} {row.get('state')} "
                          f"{row.get('owner') or ''}".rstrip())
            return EXIT_OK
        if not args.probe or args.runs is None:
            ap.error("--probe and --runs are required unless --status is given")

        def announce(index: int, total: int, port: int) -> None:
            print(f"[{index}/{total}] {args.probe} on port {port} ...",
                  file=sys.stderr, flush=True)

        outcome = run_claimed_measurement(
            args.probe, args.runs,
            artifact_root=Path(args.artifact_root) if args.artifact_root else None,
            rts_caps=args.rts_caps, lease_seconds=args.lease_seconds,
            result_path=args.result, announce=announce)
    except probe_flake.Rejection as error:
        print(f"probe_claim: {error}", file=sys.stderr)
        return EXIT_REJECTED
    except ClaimError as error:
        print(f"probe_claim: {error}", file=sys.stderr)
        return EXIT_REJECTED
    except ClaimAuditFailed as error:
        print(f"probe_claim: {error}", file=sys.stderr)
        return EXIT_CLAIM_AUDIT
    except ClaimLostDuringRun as error:
        print(f"probe_claim: {error}", file=sys.stderr)
        return EXIT_CLAIM_LOST
    except ResultIngestionFailed as error:
        print(f"probe_claim: {error}", file=sys.stderr)
        return EXIT_CLAIM_AUDIT
    except probe_flake.PortExhausted as error:
        print(f"probe_claim: {error}", file=sys.stderr)
        return EXIT_NO_PORT
    except (probe_census.CensusError,
            probe_census.DocsWorktreeMissing) as error:
        print(f"probe_claim: {error}", file=sys.stderr)
        return EXIT_CLAIM_AUDIT

    if outcome.denied is not None:
        print(f"probe_claim: {outcome.detail}", file=sys.stderr)
        if args.json:
            print(json.dumps(outcome.to_document(), indent=2, sort_keys=True))
        return outcome.exit_code

    if args.json:
        print(json.dumps(outcome.to_document(), indent=2, sort_keys=True))
    else:
        print(probe_flake.render(outcome.measurement))
        print(f"\nclaim {outcome.claim.token} recorded in {outcome.census_path}")
        if outcome.result_path is not None:
            print(f"wrote {probe_flake.RESULT_SCHEMA} to "
                  f"{outcome.result_path}")
    if outcome.result_problem:
        print(f"probe_claim: WARNING — {outcome.result_problem}",
              file=sys.stderr)
    if outcome.claim_lost:
        print(f"probe_claim: WARNING — the claim was lost during the "
              f"measurement ({outcome.claim_lost})", file=sys.stderr)
    return outcome.exit_code


if __name__ == "__main__":
    sys.exit(main())
