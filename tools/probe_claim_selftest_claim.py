#!/usr/bin/env python3
"""Atomic claim and lease lifecycle cases (#1434), split out by #2100.

The twelve cases covering repository-common namespace resolution and key
validation, exclusive acquisition, cross-process contention, expiry,
renewal, stale reclaim, owner-safe release, contended acquisition
timing, malformed claims, managed exit, crash recovery and the renewer.

The concurrency here is process-based on purpose: a claim that has to
hold between OS processes cannot be proved by threads, so these cases
start real interpreters that block on a shared barrier file before
racing, and the crash case SIGKILLs a holder and waits out its lease.

Not a gate of its own. Run through the aggregate:

  python3 tools/test_probe_claim.py --only claim
"""
from __future__ import annotations

import json
import os
import signal
import subprocess
import sys
import time
from datetime import timedelta
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import probe_claim  # type: ignore  # noqa: E402
from probe_claim_selftest_support import (  # noqa: E402
    CONTENDER, LOCK_HOLDER, claim_root, expect, expect_raises, race,
    registry, scratch, scratch_repo)


def test_namespace() -> None:
    """One repository, one claim namespace, whatever worktree asks."""
    print("\n-- the claim namespace is repository-common --")
    with registry(), scratch_repo() as (main_wt, other_wt, _census):
        here = probe_claim.repository_claim_root(str(main_wt))
        there = probe_claim.repository_claim_root(str(other_wt))
        expect(here == there,
               "a linked worktree resolves the SAME claim directory as the "
               "main checkout")
        expect(here.is_absolute() and here.name == probe_claim.CLAIM_DIR_NAME,
               "the namespace is an absolute path under the common git dir")
        expect(str(here).startswith(str(Path(main_wt).resolve())),
               "it lives under the MAIN checkout's git directory, not the "
               "asking worktree's private one")

        # None of the three things that would split the namespace move it.
        saved = dict(os.environ)
        try:
            os.environ["TMPDIR"] = str(other_wt)
            expect(probe_claim.repository_claim_root(str(other_wt)) == here,
                   "TMPDIR does not move the namespace")
        finally:
            os.environ.clear()
            os.environ.update(saved)

    with registry():
        expect_raises(probe_claim.ClaimError,
                      lambda: probe_claim.require_probe_key("alpha_probe.py"),
                      "a non-canonical spelling is refused, not claimed twice",
                      "unknown probe")
        expect_raises(probe_claim.ClaimError,
                      lambda: probe_claim.require_probe_key("../escape"),
                      "a key with path structure never reaches a path",
                      "unknown probe")
        expect(probe_claim.require_probe_key("alpha") == "alpha",
               "a canonical key is accepted")


def test_exclusive_acquisition() -> None:
    """One winner per probe; distinct probes never contend."""
    print("\n-- an exclusive claim, per probe --")
    with registry(), claim_root() as root:
        first = probe_claim.acquire("alpha", root=root, lease_seconds=600)
        expect(first.token and probe_claim.claim_path("alpha", root).exists(),
               "the first claimant creates the claim file")

        try:
            probe_claim.acquire("alpha", root=root, lease_seconds=600)
            expect(False, "a second claimant on the same probe is denied")
        except probe_claim.ClaimDenied as denied:
            expect(True, "a second claimant on the same probe is denied")
            expect(denied.owner == first.payload["owner"]
                   and denied.token == first.token,
                   "the denial names the current owner")
            expect(denied.age_seconds is not None
                   and denied.expires_in_seconds is not None,
                   "the denial reports the claim's age and remaining lease")
            expect(denied.to_document()["outcome"] == "already-claimed",
                   "the denial is a distinct machine-readable outcome")

        second = probe_claim.acquire("beta", root=root, lease_seconds=600)
        expect(second.token != first.token,
               "a DIFFERENT probe is claimable while the first is held")
        expect(probe_claim.read_claim("alpha", root=root)["token"] == first.token
               and probe_claim.read_claim("beta", root=root)["token"] == second.token,
               "the two claims are independent records")

        expect(first.release() is True, "the holder releases its own claim")
        expect(probe_claim.read_claim("alpha", root=root) is None,
               "a released claim is gone")
        third = probe_claim.acquire("alpha", root=root, lease_seconds=600)
        expect(third.token != first.token,
               "the probe is claimable again after release")


def test_independent_process_contention() -> None:
    """Separate PROCESSES racing for one probe: exactly one wins."""
    print("\n-- separate processes contend for one probe --")
    with scratch() as base:
        root = base / "probe-claims"
        rows = race(root, "alpha", 6)
        winners = [r for r in rows if r["outcome"] == "won"]
        denied = [r for r in rows if r["outcome"] == "denied"]
        expect(len(winners) == 1,
               "exactly one of six processes won the claim"
               + ("" if len(winners) == 1 else f" (got {len(winners)}: {rows})"))
        expect(len(denied) == len(rows) - 1,
               "every other process got the already-claimed outcome")
        expect(all("already claimed" in r["detail"] for r in denied),
               "each denial says the probe is already claimed")

        # Two DISTINCT probes are claimed concurrently by design: the
        # claim prevents duplicated work, it does not serialize the lab.
        both = race(root, "beta", 1) + race(root, "gamma", 1)
        expect([r["outcome"] for r in both] == ["won", "won"],
               "two distinct probes acquire concurrently")


def test_expiry_and_reclaim() -> None:
    """A lapsed claim is reclaimable; a renewed one is not."""
    print("\n-- lease expiry, renewal and reclaim --")
    with registry(), claim_root() as root:
        held = probe_claim.acquire("alpha", root=root, lease_seconds=600)
        later = probe_claim.utc_now() + timedelta(seconds=599)
        expect_raises(probe_claim.ClaimDenied,
                      lambda: probe_claim.acquire("alpha", root=root,
                                                  lease_seconds=600, now=later),
                      "one second before expiry the claim still holds")
        past = probe_claim.utc_now() + timedelta(seconds=601)
        successor = probe_claim.acquire("alpha", root=root, lease_seconds=600,
                                        now=past)
        expect(successor.token != held.token,
               "one second after expiry the claim is reclaimable")

        # Renewal is what keeps a long, SUPPORTED measurement safe.
        renewed = probe_claim.acquire("beta", root=root, lease_seconds=600)
        renewed.renew(now=probe_claim.utc_now() + timedelta(seconds=590))
        still_live = probe_claim.utc_now() + timedelta(seconds=1000)
        expect_raises(probe_claim.ClaimDenied,
                      lambda: probe_claim.acquire("beta", root=root,
                                                  lease_seconds=600,
                                                  now=still_live),
                      "a renewed claim outlives its original lease")
        expired = probe_claim.utc_now() + timedelta(seconds=1200)
        taken = probe_claim.acquire("beta", root=root, lease_seconds=600,
                                    now=expired)
        expect(taken.token != renewed.token,
               "renewal extends the lease, it does not make it permanent")


def test_concurrent_stale_reclaimers() -> None:
    """Several processes reclaiming ONE lapsed claim yield one successor."""
    print("\n-- concurrent stale reclaimers --")
    with registry(), scratch() as base:
        root = base / "probe-claims"
        root.mkdir(parents=True)
        # A claim whose lease elapsed a moment ago, exactly as a crashed
        # agent would have left it.
        stale = probe_claim.acquire("alpha", root=root, lease_seconds=1,
                                    now=probe_claim.utc_now() - timedelta(seconds=30))
        time.sleep(0.05)
        rows = race(root, "alpha", 6, lease=600.0)
        winners = [r for r in rows if r["outcome"] == "won"]
        expect(len(winners) == 1,
               "exactly one reclaimer succeeded"
               + ("" if len(winners) == 1 else f" (got {len(winners)}: {rows})"))
        survivor = probe_claim.read_claim("alpha", root=root)
        expect(survivor is not None
               and survivor["token"] == winners[0]["token"],
               "the surviving claim is the one winner's, not a later "
               "reclaimer's overwrite")
        expect(survivor["token"] != stale.token,
               "the lapsed owner's claim was genuinely replaced")


def test_owner_safe_late_release() -> None:
    """An expired owner that exits late leaves the successor alone."""
    print("\n-- ownership-safe late release --")
    with registry(), claim_root() as root:
        lapsed = probe_claim.acquire("alpha", root=root, lease_seconds=1)
        successor = probe_claim.acquire(
            "alpha", root=root, lease_seconds=600,
            now=probe_claim.utc_now() + timedelta(seconds=60))
        expect(lapsed.release() is False,
               "the expired owner's release removes nothing")
        current = probe_claim.read_claim("alpha", root=root)
        expect(current is not None and current["token"] == successor.token,
               "the successor's claim survives its predecessor's late exit")
        expect_raises(probe_claim.ClaimLost, lapsed.renew,
                      "and its late RENEWAL is refused rather than overwriting "
                      "the successor's lease",
                      "no longer ours")
        expect(probe_claim.read_claim("alpha", root=root)["token"]
               == successor.token,
               "the successor's token is still the one on disk")


def test_a_contended_acquisition_gets_a_live_lease() -> None:
    """Waiting for the lock must not eat the lease it was waiting for.

    A review found this: `acquire` sampled the clock and built its
    payload BEFORE taking the sidecar lock. Waiting for that lock takes
    as long as the writer ahead of us, which can exceed the lease
    outright — so the claim written afterwards carried an `expires_at`
    already in the past, and a claim DENIED against could have expired
    while we waited. The instant is read inside the hold now.

    Posed with a real second process holding the lock for longer than
    the whole lease, which is the only arrangement that tells the two
    orderings apart.
    """
    print("\n-- a contended acquisition still gets a live lease --")
    with registry(), scratch() as base:
        root = base / "probe-claims"
        root.mkdir(parents=True)
        ready = base / "holding"
        lease, held = 1.0, 3.0
        holder = subprocess.Popen(
            [sys.executable, "-c", LOCK_HOLDER, str(root), "alpha",
             str(held), str(ready)],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        try:
            deadline = time.time() + 60
            while time.time() < deadline and not ready.exists():
                time.sleep(0.01)
            expect(ready.exists(),
                   "the other process really is holding the claim lock, so "
                   "this acquisition genuinely waits for it")
            started = probe_claim.utc_now()
            claim = probe_claim.acquire("alpha", root=root,
                                        lease_seconds=lease)
        finally:
            holder.communicate(timeout=60)

        waited = (probe_claim.utc_now() - started).total_seconds()
        expect(waited > lease,
               f"the wait ({waited:.1f}s) really did outlast the {lease}s "
               f"lease, which is what makes this case discriminating")
        stored = probe_claim.read_claim("alpha", root=root)
        expires = probe_claim.parse_stamp(stored["expires_at"])
        acquired = probe_claim.parse_stamp(stored["acquired_at"])
        expect(probe_claim.utc_now() < expires,
               "the claim it finally took is LIVE, not born expired")
        expect(acquired >= started + timedelta(seconds=lease),
               "its timestamps were read after the wait, not before it")
        expect(abs((expires - acquired).total_seconds() - lease) < 0.05,
               "and it got its full lease, not what was left of one")
        expect(claim.holds(), "so the holder really holds it")


def test_expiry_is_one_way() -> None:
    """An expired claim cannot be renewed back to life by its own holder.

    A review found this: renewal checked only the TOKEN, so a process
    that stalled past its own lease — suspended, swapped out, stopped in
    a debugger — had a renewer that woke up eventually and revived a
    claim that had already lapsed. Nobody need have taken the probe for
    that to be wrong: it denies a claimant entitled to reclaim it, and
    it makes the lease mean nothing whenever the holder is merely slow.
    """
    print("\n-- expiry is one-way, even with no successor --")
    with registry(), claim_root() as root:
        claim = probe_claim.acquire("alpha", root=root, lease_seconds=600)
        past = probe_claim.utc_now() + timedelta(seconds=601)

        # Nobody else has touched it. The token still matches. It is
        # still expired, and that is the whole point.
        expect(probe_claim.read_claim("alpha", root=root)["token"]
               == claim.token,
               "the lapsed claim is still on disk, still carrying our token")
        expect_raises(probe_claim.ClaimLost,
                      lambda: claim.renew(now=past),
                      "an expired claim refuses to renew even with no "
                      "successor", "expired", "acquired again")
        expect_raises(probe_claim.ClaimLost,
                      lambda: claim.reassert(now=past),
                      "and refuses to reassert")
        expect(probe_claim.read_claim("alpha", root=root)["expires_at"]
               == claim.payload["expires_at"],
               "...having moved the stored expiry not one microsecond")

        # And the reclaim it was blocking really does succeed.
        successor = probe_claim.acquire("alpha", root=root, lease_seconds=600,
                                        now=past)
        expect(successor.token != claim.token,
               "so the probe is reclaimable, which is what the refusal "
               "protects")

        # A live claim renews exactly as before — a rule that refused
        # every renewal would be a different bug wearing this one's face.
        live = probe_claim.acquire("beta", root=root, lease_seconds=600)
        before = probe_claim.read_claim("beta", root=root)["expires_at"]
        live.renew(now=probe_claim.utc_now() + timedelta(seconds=599))
        expect(probe_claim.read_claim("beta", root=root)["expires_at"] > before,
               "a claim renewed one second before expiry still extends")

        # The renewer thread reports the loss rather than reviving it.
        stalled = probe_claim.acquire("gamma", root=root, lease_seconds=0.3)
        time.sleep(0.5)
        with probe_claim.Renewer(stalled, interval=0.05) as renewer:
            time.sleep(0.3)
        expect(renewer.lost is not None and renewer.renewals == 0,
               "a stalled holder's renewer reports the loss instead of "
               "reviving the lease")


def test_malformed_claim() -> None:
    """Empty, partial and unparseable claims are occupied until they age out."""
    print("\n-- a malformed claim is occupied, then reclaimable --")
    payloads = {
        "empty": "",
        "truncated": '{"schema": "probe-claim/v1", "probe": "alph',
        "not an object": '["probe-claim/v1"]',
        "no token": '{"schema": "probe-claim/v1", "probe": "alpha", '
                    '"expires_at": "2099-01-01T00:00:00Z"}',
        "unparseable expiry": '{"schema": "probe-claim/v1", "probe": "alpha", '
                              '"token": "t", "expires_at": "soon"}',
        # A claim file that disagrees about which probe it is for is a
        # copied or hand-edited one; honouring it would key the lock on
        # the filename while reporting somebody else's owner.
        "wrong probe": '{"schema": "probe-claim/v1", "probe": "beta", '
                       '"token": "t", "expires_at": "2099-01-01T00:00:00Z"}',
    }
    # PARTIAL claims: valid JSON, a live-looking far-future expiry, and
    # one required field missing each. Checking only the fields an
    # ownership decision happens to READ would accept every one of these
    # as a live claim FOREVER — never aged out, because ageing out is
    # what happens to malformed claims — so a stray or truncated write
    # would wedge the probe permanently. Each is built from a complete
    # payload so exactly one thing is wrong with it.
    # Internally CONSISTENT — `expires_at` really is `renewed_at` plus
    # `lease_seconds` — so each case below isolates exactly the one
    # thing it changes rather than being rejected for a second reason.
    complete = {"schema": "probe-claim/v1", "probe": "alpha", "token": "t",
                "owner": "dev@host:1", "host": "host", "pid": 1,
                "worktree": "/repo",
                "acquired_at": "2099-01-01T00:00:00.000000Z",
                "renewed_at": "2099-01-01T00:00:00.000000Z",
                "expires_at": "2099-01-01T01:00:00.000000Z",
                "lease_seconds": 3600.0}
    for missing in ("schema", "owner", "host", "pid", "worktree",
                    "acquired_at", "renewed_at", "lease_seconds"):
        payloads[f"partial: no {missing}"] = json.dumps(
            {f: v for f, v in complete.items() if f != missing})
    payloads["wrong schema"] = json.dumps({**complete,
                                           "schema": "probe-claim/v99"})
    payloads["boolean pid"] = json.dumps({**complete, "pid": True})
    payloads["zero lease"] = json.dumps({**complete, "lease_seconds": 0})
    payloads["oversized lease"] = json.dumps(
        {**complete, "lease_seconds": probe_claim.MAX_LEASE_SECONDS + 1})
    # INTERNALLY INCONSISTENT, and this is the shape that would wedge a
    # probe forever: every field well-typed, a one-second lease, and an
    # expiry years away. It never expires, and it never ages out either,
    # because ageing out is what happens to malformed claims.
    payloads["a one-second lease expiring in 2099"] = json.dumps(
        {**complete, "lease_seconds": 1.0})
    payloads["an expiry before its own renewal"] = json.dumps(
        {**complete, "expires_at": "2098-01-01T00:00:00.000000Z"})
    payloads["acquired after it was renewed"] = json.dumps(
        {**complete, "acquired_at": "2099-06-01T00:00:00.000000Z"})
    for name, text in payloads.items():
        with registry(), claim_root() as root:
            root.mkdir(parents=True, exist_ok=True)
            path = probe_claim.claim_path("alpha", root)
            path.write_text(text, encoding="utf-8")
            os.utime(path, (time.time() - 100, time.time() - 100))
            expect_raises(
                probe_claim.ClaimDenied,
                lambda: probe_claim.acquire("alpha", root=root,
                                            lease_seconds=600),
                f"a {name!r} claim 100s old is treated as HELD",
                "unreadable")
            # Reported rather than raised: a regression here means the
            # probe is WEDGED, which every later case needs to be able
            # to say too instead of dying on the first one.
            try:
                taken = probe_claim.acquire("alpha", root=root,
                                            lease_seconds=60)
                reclaimed = bool(taken.token) and probe_claim.read_claim(
                    "alpha", root=root)["token"] == taken.token
                detail = ""
            except probe_claim.ClaimDenied as denied:
                reclaimed, detail = False, f" (still denied: {denied})"
            expect(reclaimed,
                   f"a {name!r} claim older than the lease is "
                   f"reclaimable{detail}")


def test_release_on_every_managed_exit() -> None:
    """Normal completion, a raised exception and an interruption all release."""
    print("\n-- release on normal and abnormal (but managed) exit --")
    with registry(), claim_root() as root:
        with probe_claim.acquire("alpha", root=root, lease_seconds=600):
            pass
        expect(probe_claim.read_claim("alpha", root=root) is None,
               "a normal completion releases the claim")

        class Boom(Exception):
            pass

        for kind in (Boom, KeyboardInterrupt, SystemExit):
            try:
                with probe_claim.acquire("alpha", root=root, lease_seconds=600):
                    raise kind("stopped")
            except BaseException:  # noqa: BLE001
                pass
            expect(probe_claim.read_claim("alpha", root=root) is None,
                   f"a {kind.__name__} inside the claim still releases it")


def test_crash_recovery_through_ttl() -> None:
    """SIGKILL cannot release, so the lease is what recovers the probe."""
    print("\n-- crash/SIGKILL recovery is the lease, not cleanup --")
    with registry(), scratch() as base:
        root = base / "probe-claims"
        root.mkdir(parents=True)
        barrier = base / "go"
        barrier.write_text("go", encoding="utf-8")
        holder = subprocess.Popen(
            [sys.executable, "-c", CONTENDER, str(root), "alpha", str(barrier),
             "3", "600"], stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            text=True)
        deadline = time.time() + 60
        while time.time() < deadline:
            if probe_claim.read_claim("alpha", root=root) is not None:
                break
            time.sleep(0.05)
        held = probe_claim.read_claim("alpha", root=root)
        expect(held is not None, "the child took the claim")
        os.kill(holder.pid, signal.SIGKILL)
        holder.wait(timeout=30)
        expect(probe_claim.read_claim("alpha", root=root) is not None,
               "a SIGKILLed holder's claim SURVIVES, exactly as intended — "
               "it does not vanish with the process")
        expect_raises(probe_claim.ClaimDenied,
                      lambda: probe_claim.acquire("alpha", root=root,
                                                  lease_seconds=600),
                      "so the probe stays unavailable immediately after the "
                      "crash")
        recovered = probe_claim.acquire(
            "alpha", root=root, lease_seconds=3,
            now=probe_claim.utc_now() + timedelta(seconds=10))
        expect(recovered.token != held["token"],
               "and becomes reclaimable once the lease elapses")


def test_renewer_keeps_a_long_measurement_alive() -> None:
    """The renewer thread refreshes a live claim on its own clock."""
    print("\n-- the renewer holds a long measurement's claim --")
    with registry(), claim_root() as root:
        claim = probe_claim.acquire("alpha", root=root, lease_seconds=2)
        first = probe_claim.read_claim("alpha", root=root)["expires_at"]
        with probe_claim.Renewer(claim, interval=0.05) as renewer:
            time.sleep(1.2)
        expect(renewer.renewals > 0 and renewer.lost is None,
               f"the renewer refreshed the lease ({renewer.renewals} times)")
        current = probe_claim.read_claim("alpha", root=root)
        expect(current is not None and current["token"] == claim.token,
               "the claim is still ours after the renewals")
        expect(current["expires_at"] >= first,
               "and its expiry moved forward rather than back")

        # A claim taken away underneath the renewer is REPORTED, never
        # silently re-taken: it means two agents may have measured one probe.
        claim.release()
        other = probe_claim.acquire("alpha", root=root, lease_seconds=600)
        with probe_claim.Renewer(claim, interval=0.05) as lost_renewer:
            time.sleep(0.4)
        expect(lost_renewer.lost is not None,
               "a claim lost mid-measurement is reported")
        expect(probe_claim.read_claim("alpha", root=root)["token"] == other.token,
               "and the new owner's claim is untouched")


#: This owner's inventory, in the relative order these cases hold within
#: the aggregate's run sequence. The aggregate composes that sequence
#: from all three inventories; nothing here decides when it runs.
CASES = (
    test_namespace,
    test_exclusive_acquisition,
    test_independent_process_contention,
    test_expiry_and_reclaim,
    test_concurrent_stale_reclaimers,
    test_owner_safe_late_release,
    test_a_contended_acquisition_gets_a_live_lease,
    test_expiry_is_one_way,
    test_malformed_claim,
    test_release_on_every_managed_exit,
    test_crash_recovery_through_ttl,
    test_renewer_keeps_a_long_measurement_alive,
)
