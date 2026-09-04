#!/usr/bin/env python3
"""Who owns a probe right now: acquisition, the lease, and its end (#2148).

The ownership owner of `tools/probe_claim.py`'s three. It builds on
`probe_claim_storage` for everything on disk — the namespace, the
codec, the durable writes and the sidecar `flock` — and adds the only
question that module refuses to answer: is this claim live, is it ours,
and may it be taken?

Identity
--------
Claims are keyed by the canonical `probe_runner_registry.PROBES` key
and nothing else, so the two spellings of a probe cannot claim it
twice. Validating against the registry also means no claim path is ever
built from an unvetted string.

The lease, and why it is renewed rather than sized
--------------------------------------------------
No fixed TTL can be both safe and correct here: `probe_flake.measure`
accepts any positive `--runs` and each run inherits a 900-second
timeout, so a supported ten-run measurement legitimately outlives any
constant. A live claim is therefore RENEWED — `Renewer` refreshes the
lease while the measurement runs, and `LEASE_SECONDS` only has to
exceed one run's worst case, which it does with a wide margin. A
long-running supported measurement never becomes reclaimable; a dead
holder's claim lapses one lease after its last renewal.

`acquire` accepts any positive lease, so a test can drive expiry
deliberately; the FLOOR that keeps a lease from elapsing mid-run is
`probe_claim_orchestration.MIN_ORCHESTRATION_LEASE_SECONDS`, enforced
one owner up.

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

A claim file that is empty, truncated, unparseable or INCOMPLETE comes
back from `probe_claim_storage.read_payload` as `None` beside its own
mtime. This owner treats it as OCCUPIED until that filesystem age
reaches the lease in force for the acquisition, then takes it over —
which covers a crash between the exclusive create and the payload write
without opening a window in which a competing acquisition succeeds
immediately. Storage exposes the age; the denial is decided here,
because `ClaimDenied` is this module's.

Every collaborator of `probe_claim_storage`'s is reached
MODULE-QUALIFIED at call time (`storage.read_payload(...)`), never
through a `from ... import`: a from-import binds the name here at
import time, and a test or tool that substitutes the storage owner's
function would then be changing state nothing reads.

Not a program. `tools/probe_claim.py` remains the only command.
"""
from __future__ import annotations

import math
import os
import socket
import sys
import threading
import uuid
from datetime import datetime, timedelta
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_claim_storage as storage  # noqa: E402
import probe_engine  # noqa: E402
import probe_runner_registry  # noqa: E402

# One run's worst case is `probe_runner_registry.DEFAULT_TIMEOUT` (900 s). The lease
# only has to outlive ONE run, because the renewer refreshes it between
# runs and on its own clock; the wide margin is what keeps a stalled
# renewer thread from dropping a live claim.
LEASE_SECONDS = 4.0 * probe_runner_registry.DEFAULT_TIMEOUT
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


# ----------------------------------------------------------------------
# Validation
# ----------------------------------------------------------------------
def require_probe_key(probe) -> str:
    """The canonical `probe_runner_registry.PROBES` key, or a controlled refusal.

    Claims are keyed by the canonical key and only by it, so the several
    human spellings of one probe cannot claim it twice. Validating
    against the registry also means no claim path is ever built from an
    unvetted string.
    """
    if not isinstance(probe, str) or not probe:
        raise storage.ClaimError(
            f"a probe key must be a non-empty string, got {probe!r}")
    known = {key for key, _script, _purpose in probe_runner_registry.PROBES}
    if probe not in known:
        raise storage.ClaimError(
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
    `probe_claim_storage.MAX_LEASE_SECONDS` is where the census's own
    cap sits, so a lease this accepts is one the acquisition record can
    also hold.

    `True` is excluded explicitly, because `isinstance(True, int)`.
    """
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise storage.ClaimError(
            f"{what} must be a number of seconds, got {value!r}")
    if not math.isfinite(value):
        raise storage.ClaimError(
            f"{what} must be a finite number of seconds, got {value!r}")
    if value <= 0:
        raise storage.ClaimError(
            f"{what} must be a positive number of seconds, got {value!r}")
    if value > storage.MAX_LEASE_SECONDS:
        raise storage.ClaimError(
            f"{what} must be at most {storage.MAX_LEASE_SECONDS:.0f} seconds "
            f"(about thirty-one years), got {value!r}")
    return float(value)


# ----------------------------------------------------------------------
# The claim payload
# ----------------------------------------------------------------------
def _owner_description() -> str:
    user = os.environ.get("USER") or os.environ.get("LOGNAME") or f"uid{os.getuid()}"
    return f"{user}@{socket.gethostname()}:{os.getpid()}"


def _payload(probe: str, token: str, *, acquired: datetime, renewed: datetime,
             lease_seconds: float, owner: str, worktree: str) -> dict:
    return {
        "schema": storage.CLAIM_SCHEMA,
        "probe": probe,
        "token": token,
        "owner": owner,
        "host": socket.gethostname(),
        "pid": os.getpid(),
        "worktree": worktree,
        "acquired_at": storage.stamp(acquired),
        "renewed_at": storage.stamp(renewed),
        "expires_at": storage.stamp(
            renewed + timedelta(seconds=lease_seconds)),
        "lease_seconds": lease_seconds,
    }


# ----------------------------------------------------------------------
# Ownership
# ----------------------------------------------------------------------
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
        except storage.ClaimError:
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
        document, _mtime = storage.read_payload(self.path, self.probe)
        if document is None or document.get("token") != self.token:
            raise ClaimLost(
                f"the claim on {self.probe!r} is no longer ours "
                f"(token {self.token!r} is not the one on disk)")
        expires = storage.parse_stamp(document.get("expires_at"))
        if expires is None or moment >= expires:
            raise ClaimLost(
                f"the claim on {self.probe!r} expired at "
                f"{document.get('expires_at')!r} and cannot be renewed; it "
                f"has to be acquired again")
        payload = dict(document)
        payload["renewed_at"] = storage.stamp(moment)
        payload["expires_at"] = storage.stamp(
            moment + timedelta(seconds=self.lease_seconds))
        payload["lease_seconds"] = self.lease_seconds
        storage.install(self.path, payload)
        self.payload = payload
        return payload

    def _holds_locked(self, moment: datetime) -> bool:
        """`holds`'s body. The sidecar lock must already be held."""
        document, _mtime = storage.read_payload(self.path, self.probe)
        if document is None or document.get("token") != self.token:
            return False
        expires = storage.parse_stamp(document.get("expires_at"))
        return expires is not None and moment < expires

    def renew(self, now: datetime | None = None) -> dict:
        """Extend this claim's lease. Token-checked.

        Raises `ClaimLost` when the claim is gone or now carries another
        token: an expired owner must never write over its successor.
        """
        with storage.serialized(self.probe, self.root):
            return self._renew_locked(
                now if now is not None else storage.utc_now())

    def release(self) -> bool:
        """Give the claim back. Token-checked; True when we removed it.

        A claim carrying someone ELSE's token is left exactly where it
        is. That is the late-release case: our lease lapsed, a successor
        took the probe, and this process is only now unwinding. Deleting
        that file would hand one probe to two agents at once.
        """
        if self.released:
            return False
        with storage.serialized(self.probe, self.root):
            document, _mtime = storage.read_payload(self.path, self.probe)
            self.released = True
            if document is None or document.get("token") != self.token:
                return False
            try:
                self.path.unlink()
            except FileNotFoundError:
                return False
            except OSError as error:
                raise storage.ClaimError(
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
        with storage.serialized(self.probe, self.root):
            return self._holds_locked(
                now if now is not None else storage.utc_now())

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
        with storage.serialized(self.probe, self.root):
            # Read INSIDE the hold: waiting for the lock can take as
            # long as the writer ahead of us, and an instant sampled
            # before that wait is stale in the permissive direction.
            moment = now if now is not None else storage.utc_now()
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
        acquired = (storage.parse_stamp(self.payload["acquired_at"])
                    or storage.utc_now())
        return {
            "token": self.token,
            "timestamp_utc": storage.stamp_second(acquired),
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
    base = storage.ensure_root(Path(root) if root is not None
                        else storage.repository_claim_root(repo_root))
    path = storage.claim_path(key, base)
    token = uuid.uuid4().hex

    with storage.serialized(key, base):
        # The clock is read INSIDE the hold, and the payload is built
        # from it here rather than above. Waiting for this lock takes as
        # long as the writer ahead of us, which can exceed the lease
        # outright — and an instant sampled before that wait is wrong in
        # both directions at once: the claim we go on to write would be
        # stamped with an `expires_at` already in the past, and a claim
        # we go on to DENY against might have expired while we waited.
        # `now` stays injectable so the self-test can pose a specific
        # instant; nothing else may pre-sample one.
        moment = now if now is not None else storage.utc_now()
        payload = _payload(key, token, acquired=moment, renewed=moment,
                           lease_seconds=float(lease_seconds),
                           owner=_owner_description(),
                           worktree=str(Path(repo_root
                                             or probe_engine.REPO_ROOT)))
        document, mtime = storage.read_payload(path, key)
        if document is None and mtime is None:
            if storage.create_exclusive(path, payload):
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
            storage.install(path, payload)
            return Claim(key, token, path, base, payload, float(lease_seconds))
        expires = storage.parse_stamp(document.get("expires_at"))
        if expires is not None and moment < expires:
            acquired = storage.parse_stamp(document.get("acquired_at"))
            raise ClaimDenied(
                key,
                owner=document.get("owner"),
                host=document.get("host"),
                pid=document.get("pid"),
                token=document.get("token"),
                age_seconds=((moment - acquired).total_seconds()
                             if acquired is not None else None),
                expires_in_seconds=(expires - moment).total_seconds())
        storage.install(path, payload)
        return Claim(key, token, path, base, payload, float(lease_seconds))


def read_claim(probe: str, *, root: Path | None = None,
               repo_root: str | None = None):
    """The current claim on `probe` as stored, or None. Read-only."""
    key = require_probe_key(probe)
    base = (Path(root) if root is not None
            else storage.repository_claim_root(repo_root))
    if not base.is_dir():
        return None
    document, _mtime = storage.read_payload(storage.claim_path(key, base), key)
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
            except storage.ClaimError as error:
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


# ----------------------------------------------------------------------
# Status
# ----------------------------------------------------------------------
def status_rows(root: Path) -> list[dict]:
    """Every registered probe's current claim state, as plain rows.

    The status QUERY, so the command that renders `--status` never has
    to read a claim file itself: the row shapes below are exactly what
    it prints and what `--json` publishes, and deciding `held` from
    `lapsed` is an ownership judgement, which is this owner's.

    One row per probe that has a claim file at all. `unusable` is a
    refusal storage raised about the file itself; `malformed` carries
    the filesystem age an acquisition would age it out against.
    """
    rows = []
    if not root.is_dir():
        return rows
    now = storage.utc_now()
    for key, _script, _purpose in probe_runner_registry.PROBES:
        path = storage.claim_path(key, root)
        try:
            document, mtime = storage.read_payload(path, key)
        except storage.ClaimError as error:
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
        expires = storage.parse_stamp(document.get("expires_at"))
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
