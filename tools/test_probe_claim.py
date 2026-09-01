#!/usr/bin/env python3
"""Focused self-test for the atomic per-probe claim (#1434).

Deterministic, engine-free, GPU-free and offline. Nothing here boots an
engine, runs a registered probe, touches the developer's real `docs-wip`
worktree or the repository's real claim namespace: every case runs in a
throwaway temporary tree, and the only subprocesses are `git` (to build
scratch repositories with real linked worktrees) and this same
interpreter (for the cases that need genuinely separate processes).

The real `tools/probe_claim.py` and `tools/probe_census.py` are imported
and driven, with `probe_runner_registry.PROBES` and the other live registries
pointed at a synthetic set, so this exercises the shipped code paths
rather than a copy.

What the concurrency cases actually prove, and why they are subprocesses
rather than threads: the claim has to hold between OS processes, and an
in-process test could pass on nothing but the GIL. Each of them starts N
real interpreters that block on a shared barrier file before racing, so
the contention is real and the count of winners is the assertion.

Usage:
  python3 tools/test_probe_claim.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import atexit
import json
import os
import shutil
import signal
import subprocess
import sys
import tempfile
import time
from contextlib import contextmanager
from datetime import timedelta
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import ci_probes  # type: ignore  # noqa: E402
import probe_census  # type: ignore  # noqa: E402
import probe_claim  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
import probe_protocol  # type: ignore  # noqa: E402
import probe_engine  # type: ignore  # noqa: E402
import probe_runner_registry  # type: ignore  # noqa: E402

import selftest  # noqa: E402
from selftest import FAILURES, expect  # noqa: E402


TOOLS = str(Path(__file__).resolve().parent)
COMMIT_A = "a" * 40
COMMIT_B = "b" * 40

SYNTHETIC = [
    ("alpha", "alpha_probe.py", "the first synthetic probe"),
    ("beta", "beta_probe.py", "the second synthetic probe"),
    ("gamma", "gamma_probe.py", "the third synthetic probe"),
]


def skip(msg: str) -> None:
    """A case this environment cannot pose. Reported, never counted."""
    print(f"  skip {msg}")


def expect_raises(kind, call, msg: str, *fragments: str) -> None:
    try:
        call()
    except kind as error:
        missing = [f for f in fragments if f not in str(error)]
        expect(not missing,
               msg if not missing else f"{msg} (missing {missing} in {error})")
        return
    except Exception as error:  # noqa: BLE001
        expect(False, f"{msg} (raised {type(error).__name__}: {error})")
        return
    expect(False, f"{msg} (nothing was raised)")


# ==========================================================================
# Fixtures
# ==========================================================================
@contextmanager
def registry(probes=None, ci_eligible=(), protocol=None):
    """The live registries, pointed at a synthetic set for one case."""
    saved = (probe_runner_registry.PROBES, ci_probes.CI_ELIGIBLE,
             probe_flake.PROTOCOL_PROBES)
    probe_runner_registry.PROBES = list(SYNTHETIC if probes is None else probes)
    ci_probes.CI_ELIGIBLE = set(ci_eligible)
    probe_flake.PROTOCOL_PROBES = dict(
        {key: probe_protocol.PROTOCOL_VERSION for key, _s, _p in probe_runner_registry.PROBES}
        if protocol is None else protocol)
    try:
        yield
    finally:
        (probe_runner_registry.PROBES, ci_probes.CI_ELIGIBLE,
         probe_flake.PROTOCOL_PROBES) = saved


@contextmanager
def scratch(prefix="probe-claim-test-"):
    root = Path(tempfile.mkdtemp(prefix=prefix))
    try:
        yield root
    finally:
        shutil.rmtree(root, ignore_errors=True)


@contextmanager
def claim_root():
    with scratch() as root:
        yield root / "probe-claims"


@contextmanager
def scratch_repo():
    """A scratch repository with a linked worktree and a `docs-wip` one.

    Three checkouts of ONE repository, which is what the namespace rule
    is actually about: `main`, a second ordinary worktree, and the
    docs-wip worktree the census resolves through.
    """
    with scratch("probe-claim-repo-") as root:
        main_wt, other_wt, docs_wt = root / "main", root / "other", root / "docs"
        env = {**os.environ, "GIT_CONFIG_GLOBAL": str(root / "gitconfig"),
               "GIT_CONFIG_SYSTEM": "/dev/null"}

        def run(*args, cwd=None):
            return subprocess.run(args, cwd=str(cwd or main_wt), env=env,
                                  check=True, capture_output=True, text=True)

        subprocess.run(["git", "init", "-q", "-b", "master", str(main_wt)],
                       env=env, check=True, capture_output=True)
        run("git", "config", "user.email", "test@example.invalid")
        run("git", "config", "user.name", "Claim Test")
        run("git", "commit", "-q", "--allow-empty", "-m", "root")
        run("git", "worktree", "add", "-q", str(other_wt), "-b", "feature")
        run("git", "worktree", "add", "-q", str(docs_wt), "-b", "docs-wip")
        saved = probe_engine.REPO_ROOT
        probe_engine.REPO_ROOT = str(main_wt)
        try:
            yield main_wt, other_wt, docs_wt / probe_census.MANIFEST_RELPATH
        finally:
            probe_engine.REPO_ROOT = saved


def seeded_census(path: Path) -> Path:
    """A freshly seeded v3 census for the synthetic registry."""
    probe_census.ensure_document(path)
    return path


def descriptor(probe="alpha") -> probe_protocol.Descriptor:
    return probe_protocol.Descriptor(
        probe=probe, checks=(("first", "the first check"),
                             ("second", "the second check")))


ARTIFACTS = tempfile.mkdtemp(prefix="probe-claim-artifacts-")
atexit.register(shutil.rmtree, ARTIFACTS, True)


def fake_measurement(probe="alpha", runs=2, *, harness_error=False,
                     artifact_root=None):
    """A real `probe_flake.Measurement`, filled in without running anything.

    Its artifact root is a throwaway tree of this run's own, because a
    completed measurement is now RETAINED under its invocation
    directory — so a hard-coded root would have every case here writing
    into a shared path outside the scratch.
    """
    artifact_root = ARTIFACTS if artifact_root is None else artifact_root
    measurement = probe_flake.Measurement(
        probe, descriptor(probe), runs, 4, Path(artifact_root),
        Path(artifact_root) / "invocation")
    measurement.commit_sha = COMMIT_A
    measurement.timestamp = "2026-08-21T05:00:00Z"
    if harness_error:
        measurement.status = "harness-error"
        measurement.error = "run 2: duplicate event"
        measurement.runs = [probe_flake.RunRecord(
            1, 9100, probe_flake.RUN_PASS, 1.0,
            {"first": "PASS", "second": "PASS"}, None)]
        measurement.error_run = probe_flake.RunRecord(
            2, 9101, probe_flake.RUN_HARNESS_ERROR, 0.5, {}, None)
    else:
        measurement.runs = [
            probe_flake.RunRecord(index, 9100 + index, probe_flake.RUN_PASS,
                                  1.0, {"first": "PASS", "second": "PASS"},
                                  None)
            for index in range(1, runs + 1)]
    return measurement


CONTENDER = f"""
import json, os, sys, time
sys.path.insert(0, {TOOLS!r})
import probe_runner_registry
probe_runner_registry.PROBES = [(k, k + '_probe.py', k)
                                for k in ('alpha', 'beta', 'gamma')]
import probe_claim

root, probe, barrier, lease = sys.argv[1], sys.argv[2], sys.argv[3], float(sys.argv[4])
hold = float(sys.argv[5]) if len(sys.argv) > 5 else 0.0
# An optional future offset, so a contender can look at a claim the way
# its own clock would once the lease had elapsed.
skew = float(sys.argv[6]) if len(sys.argv) > 6 else 0.0
# Every contender waits for the same starting gun, so the race is real.
while not os.path.exists(barrier):
    time.sleep(0.01)
# "I am about to block on the claim lock": a waiter that has not reached
# the lock yet would make a still-running assertion vacuous.
open(barrier + '.ready', 'w').close()
try:
    when = probe_claim.utc_now() + __import__('datetime').timedelta(seconds=skew)
    claim = probe_claim.acquire(probe, root=__import__('pathlib').Path(root),
                                lease_seconds=lease, now=when)
except probe_claim.ClaimDenied as denied:
    print(json.dumps({{"outcome": "denied", "detail": denied.describe()}}))
    sys.exit(0)
except probe_claim.ClaimError as error:
    print(json.dumps({{"outcome": "error", "detail": str(error)}}))
    sys.exit(0)
print(json.dumps({{"outcome": "won", "token": claim.token}}), flush=True)
if hold > 0:
    time.sleep(hold)
    claim.release()
"""


LOCK_HOLDER = f"""
import os, sys, time
sys.path.insert(0, {TOOLS!r})
import probe_runner_registry
probe_runner_registry.PROBES = [(k, k + '_probe.py', k)
                                for k in ('alpha', 'beta', 'gamma')]
import probe_claim
from pathlib import Path

root, probe, held, ready = sys.argv[1], sys.argv[2], float(sys.argv[3]), sys.argv[4]
with probe_claim._serialized(probe, Path(root)):
    open(ready, 'w').close()
    time.sleep(held)
"""


def race(root: Path, probe: str, count: int, *, lease=60.0):
    """`count` separate processes contend for one probe. Returns their rows."""
    root.mkdir(parents=True, exist_ok=True)
    barrier = root.parent / f"barrier-{probe}-{count}"
    children = [subprocess.Popen(
        [sys.executable, "-c", CONTENDER, str(root), probe, str(barrier),
         str(lease)], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        for _ in range(count)]
    time.sleep(0.4)
    barrier.write_text("go", encoding="utf-8")
    rows = []
    for process in children:
        out, err = process.communicate(timeout=120)
        try:
            rows.append(json.loads(out.strip().splitlines()[-1]))
        except (IndexError, ValueError):
            rows.append({"outcome": "crashed", "detail": (err or out)[-400:]})
    return rows


# ==========================================================================
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


# ==========================================================================
# The census side
# ==========================================================================
def test_census_claim_collection() -> None:
    """Claims are their own append-only collection, keyed by token."""
    print("\n-- the census records acquisitions, idempotently --")
    with registry(), scratch_repo() as (_main, _other, census):
        seeded_census(census)
        record = {"token": "tok-1", "timestamp_utc": "2026-08-21T05:00:00Z",
                  "commit_sha": COMMIT_A, "owner": "dev@host:1",
                  "host": "host", "pid": 1, "lease_seconds": 3600.0,
                  "requested_runs": 10}
        probe_census.record_claim(census, "alpha", record)
        document = json.loads(census.read_text(encoding="utf-8"))
        row = probe_census.find_entry(document, "alpha")["census"]
        expect(row["claims"] == [record],
               "one acquisition appends exactly one claim record")
        expect(row["attempts"] == [] and row["current"] is None,
               "and touches neither the attempt log nor the measurements")

        before = census.read_bytes()
        probe_census.record_claim(census, "alpha", dict(record))
        expect(census.read_bytes() == before,
               "replaying the SAME acquisition token is a byte-for-byte no-op")

        conflicting = dict(record, requested_runs=3)
        expect_raises(
            probe_census.CensusError,
            lambda: probe_census.record_claim(census, "alpha", conflicting),
            "the same token with different metadata is refused",
            "tok-1", "different metadata")
        expect(census.read_bytes() == before,
               "...having written nothing")

        second = dict(record, token="tok-2", requested_runs=3)
        probe_census.record_claim(census, "alpha", second)
        document = json.loads(census.read_text(encoding="utf-8"))
        expect(probe_census.find_entry(document, "alpha")["census"]["claims"]
               == [record, second],
               "a genuinely new acquisition appends beside the first")

        expect_raises(
            probe_census.CensusError,
            lambda: probe_census.record_claim(census, "alpha", {"owner": "x"}),
            "a claim with no token is refused", "token")
        expect_raises(
            probe_census.CensusError,
            lambda: probe_census.record_claim(census, "nonesuch", record),
            "a claim for a probe with no census row is refused", "nonesuch")


def test_claims_are_not_measurements() -> None:
    """Neither aspect may reach into the other's fields."""
    print("\n-- the claim log and the measurement log stay separate --")
    with registry(), scratch_repo() as (_main, _other, census):
        seeded_census(census)
        record = {"token": "tok-1", "timestamp_utc": "2026-08-21T05:00:00Z",
                  "commit_sha": COMMIT_A, "owner": "dev@host:1",
                  "host": "host", "pid": 1, "lease_seconds": 60.0,
                  "requested_runs": 2}
        probe_census.record_claim(census, "alpha", record)
        probe_census.record_result(census,
                                   fake_measurement("alpha").to_document())
        document = json.loads(census.read_text(encoding="utf-8"))
        row = probe_census.find_entry(document, "alpha")["census"]
        expect(row["claims"] == [record],
               "ingesting a measurement leaves the claim log alone")
        expect(len(row["attempts"]) == 1 and row["current"] is not None,
               "and the measurement still landed")

        # The preservation guard is what makes those promises real, so
        # drive it directly from both directions.
        def claim_touching_measurements(before):
            candidate = probe_census._deep_copy(before)
            target = probe_census.find_entry(candidate, "alpha")["census"]
            target["attempts"] = target["attempts"] + [dict(target["attempts"][0])]
            return candidate, {"alpha": {"claims"}}

        def measurement_touching_claims(before):
            candidate = probe_census._deep_copy(before)
            target = probe_census.find_entry(candidate, "alpha")["census"]
            target["claims"] = target["claims"] + [dict(record, token="tok-9")]
            return candidate, {"alpha": {"measurements"}}

        for mutate, msg, fragment in (
                (claim_touching_measurements,
                 "a claim operation may not append an attempt", "attempts"),
                (measurement_touching_claims,
                 "a measurement operation may not append a claim", "claims")):
            expect_raises(probe_census.CensusError,
                          lambda m=mutate: probe_census.update(census, m),
                          msg, fragment)

        def drops_a_claim(before):
            candidate = probe_census._deep_copy(before)
            probe_census.find_entry(candidate, "alpha")["census"]["claims"] = []
            return candidate, {"alpha": {"claims"}}

        expect_raises(probe_census.CensusError,
                      lambda: probe_census.update(census, drops_a_claim),
                      "the claim log is append-only: a candidate that "
                      "discards one is refused", "append-only")


def test_schema_migration_is_lossless() -> None:
    """v1 and v2 censuses migrate keeping every accumulated field.

    The claim log arrived in `probe-census/v3`, which is the version
    this pins; the CURRENT schema moves on as later issues extend the
    record, and the migration this covers has to keep working every
    time it does.
    """
    print("\n-- the v2 -> current migration loses nothing --")
    with registry():
        expect(probe_census.CLAIM_SCHEMA == "probe-census/v3",
               "the claim log arrived in probe-census/v3")
        expect(probe_census.CENSUS_SCHEMA in probe_census.MIGRATABLE_SCHEMAS
               and probe_census.MIGRATABLE_SCHEMAS.index(
                   probe_census.CLAIM_SCHEMA)
               < probe_census.MIGRATABLE_SCHEMAS.index(
                   probe_census.CENSUS_SCHEMA),
               f"...and every later schema still migrates from it; got "
               f"{probe_census.MIGRATABLE_SCHEMAS}")

        cohort = {"commit_sha": COMMIT_A, "samples": []}
        attempt = {"timestamp_utc": "2026-08-20T05:00:00Z",
                   "commit_sha": COMMIT_A, "status": "harness-error",
                   "accepted": False, "requested_runs": 2, "completed_runs": 1,
                   "error": "run 2 broke", "retained_artifacts": []}
        v2 = {
            "schema": "probe-census/v2",
            "probes": [{
                "key": "alpha", "script": "alpha_probe.py",
                "classification": "manual-only",
                "protocol": probe_protocol.PROTOCOL_VERSION,
                "census": {
                    "acceptable_failures": 3,
                    "acceptable_failures_justification": "three known races",
                    "estimated_worst_case_seconds": 480,
                    "current": {"commit_sha": COMMIT_B, "samples": []},
                    "history": [cohort],
                    "attempts": [attempt],
                },
            }],
        }
        probe_census.validate_document(v2, "probe-census/v2", "a stored v2")
        migrated = probe_census.migrate_document(v2)
        probe_census.validate_document(migrated, probe_census.CENSUS_SCHEMA,
                                       "the migrated census")
        record = migrated["probes"][0]["census"]
        expect(migrated["schema"] == probe_census.CENSUS_SCHEMA,
               f"the migrated document is "
               f"{probe_census.CENSUS_SCHEMA}")
        expect(record["claims"] == [] and record["outcomes"] == []
               and record["deferred"] is None,
               "the migration adds an EMPTY claim log, and #1439's equally "
               "empty outcome log plus v5's null deferral beside it")
        for field, value in (("acceptable_failures", 3),
                             ("acceptable_failures_justification",
                              "three known races"),
                             ("estimated_worst_case_seconds", 480),
                             ("history", [cohort]), ("attempts", [attempt])):
            expect(record[field] == value,
                   f"the migration preserves `{field}` exactly")
        expect(record["current"] == {"commit_sha": COMMIT_B, "samples": []},
               "the migration preserves the current cohort exactly")
        expect(v2["probes"][0]["census"].get("claims") is None
               and v2["probes"][0]["census"].get("outcomes") is None
               and v2["probes"][0]["census"].get("deferred") is None,
               "and it does not mutate the document it migrated FROM")

        again = probe_census.migrate_document(migrated)
        expect(again == migrated,
               "re-migrating an already-migrated census is a no-op")

        with_claims = probe_census._deep_copy(migrated)
        kept = {"token": "tok-1", "timestamp_utc": "2026-08-21T05:00:00Z",
                "commit_sha": COMMIT_A, "owner": "dev@host:1", "host": "host",
                "pid": 1, "lease_seconds": 60.0, "requested_runs": 2}
        with_claims["probes"][0]["census"]["claims"] = [kept]
        expect(probe_census.migrate_document(with_claims)["probes"][0]
               ["census"]["claims"] == [kept],
               "re-migrating a current census never truncates its existing "
               "claims")

        v1 = {"schema": "probe-census/v1",
              "probes": [{"key": "alpha", "script": "alpha_probe.py",
                          "classification": "manual-only",
                          "protocol": "legacy"}]}
        from_v1 = probe_census.migrate_document(v1)
        probe_census.validate_document(from_v1, probe_census.CENSUS_SCHEMA,
                                       "the migrated v1 census")
        expect(from_v1["probes"][0]["census"] == probe_census.empty_census(),
               "a v1 seed still migrates straight to the empty current "
               "record")

    with registry(), scratch_repo() as (_main, _other, census):
        # The migration through the real writer, on disk, is what an
        # operator actually runs.
        census.parent.mkdir(parents=True, exist_ok=True)
        v2["probes"] = [dict(v2["probes"][0])] + [
            {"key": key, "script": script, "classification": "manual-only",
             "protocol": probe_protocol.PROTOCOL_VERSION,
             "census": probe_census.empty_census()}
            for key, script, _p in SYNTHETIC[1:]]
        for row in v2["probes"][1:]:
            row["census"].pop("claims")
            row["census"].pop("outcomes")
            row["census"].pop("deferred")
        census.write_text(json.dumps(v2, indent=2, sort_keys=True) + "\n",
                          encoding="utf-8")
        probe_census.ensure_document(census)
        stored = json.loads(census.read_text(encoding="utf-8"))
        expect(stored["schema"] == probe_census.CENSUS_SCHEMA,
               "`--seed` migrates a stored v2 census in place")
        alpha = probe_census.find_entry(stored, "alpha")["census"]
        expect(alpha["acceptable_failures"] == 3
               and alpha["history"] == [cohort] and alpha["attempts"] == [attempt],
               "...keeping every policy field, cohort and attempt it held")
        expect(all(probe_census.find_entry(stored, key)["census"]["claims"] == []
                   and probe_census.find_entry(
                       stored, key)["census"]["outcomes"] == []
                   and probe_census.find_entry(
                       stored, key)["census"]["deferred"] is None
                   for key, _s, _p in SYNTHETIC),
               "...and giving every row empty claim/outcome logs and a null "
               "deferral")


# ==========================================================================
# The orchestration boundary
# ==========================================================================
def test_orchestration_happy_path() -> None:
    """Claim, record, measure, ingest, release — in that order."""
    print("\n-- the claim-aware orchestration boundary --")
    with registry(), scratch_repo() as (main_wt, _other, census):
        seeded_census(census)
        with claim_root() as root:
            seen = {}

            def measure(probe, runs, **kwargs):
                # While the measurement runs, the claim is HELD and the
                # acquisition is already recorded.
                seen["claim"] = probe_claim.read_claim(probe, root=root)
                document = json.loads(census.read_text(encoding="utf-8"))
                seen["claims"] = probe_census.find_entry(
                    document, probe)["census"]["claims"]
                seen["attempts"] = probe_census.find_entry(
                    document, probe)["census"]["attempts"]
                return fake_measurement(probe, runs)

            outcome = probe_claim.run_claimed_measurement(
                "alpha", 2, root=root, census_path=census, measure=measure,
                repo_root=str(main_wt), renew_interval=0.05)

        expect(outcome.exit_code == probe_claim.EXIT_OK
               and outcome.outcome == "measured",
               "a completed measurement reports success")
        expect(seen["claim"] is not None
               and seen["claim"]["token"] == outcome.claim.token,
               "the claim is held while the probe runs")
        expect(len(seen["claims"]) == 1
               and seen["claims"][0]["token"] == outcome.claim.token,
               "the acquisition is recorded BEFORE the measurement begins")
        expect(seen["attempts"] == [],
               "and no result is ingested before the measurement finishes")

        document = json.loads(census.read_text(encoding="utf-8"))
        row = probe_census.find_entry(document, "alpha")["census"]
        expect(len(row["attempts"]) == 1 and row["current"] is not None,
               "the result is ingested while the claim is still held")
        expect(row["claims"][0]["requested_runs"] == 2
               and row["claims"][0]["commit_sha"] != "",
               "the claim record carries the run count and the commit")
        expect(outcome.claim.released,
               "and the claim is released once ingestion is durable")


def test_orchestration_denied_creates_nothing() -> None:
    """A denied claimant writes no artifact, no result and no census entry."""
    print("\n-- a denied claimant creates nothing --")
    with registry(), scratch_repo() as (main_wt, _other, census):
        seeded_census(census)
        with claim_root() as root:
            holder = probe_claim.acquire("alpha", root=root, lease_seconds=600)
            before = census.read_bytes()
            ran = []
            outcome = probe_claim.run_claimed_measurement(
                "alpha", 2, root=root, census_path=census,
                measure=lambda *a, **k: ran.append(1) or fake_measurement(),
                repo_root=str(main_wt))
            expect(outcome.exit_code == probe_claim.EXIT_ALREADY_CLAIMED,
                   "the run reports the already-claimed exit code")
            expect(outcome.denied is not None
                   and outcome.denied.token == holder.token,
                   "and names the holder in its outcome")
            expect(not ran, "the probe was never executed")
            expect(census.read_bytes() == before,
                   "the census is byte-for-byte unchanged")
            expect(probe_claim.read_claim("alpha", root=root)["token"]
                   == holder.token,
                   "and the holder's claim is untouched")


def test_orchestration_claim_audit_failure() -> None:
    """An unrecordable acquisition releases the claim and runs nothing."""
    print("\n-- a claim audit failure prevents execution --")
    with registry(), scratch_repo() as (main_wt, _other, census):
        # Deliberately NOT seeded: `--record` refuses an absent census by
        # name, which is exactly the audit failure this must survive.
        with claim_root() as root:
            ran = []
            expect_raises(
                probe_claim.ClaimAuditFailed,
                lambda: probe_claim.run_claimed_measurement(
                    "alpha", 2, root=root, census_path=census,
                    measure=lambda *a, **k: ran.append(1) or fake_measurement(),
                    repo_root=str(main_wt)),
                "an unrecordable acquisition refuses the measurement",
                "could not be recorded", "released")
            expect(not ran, "the probe was never executed")
            expect(probe_claim.read_claim("alpha", root=root) is None,
                   "and the claim was released, not left wedged")

        # The same, with the docs worktree itself unreachable.
        with claim_root() as root, scratch() as elsewhere:
            saved = probe_engine.REPO_ROOT
            probe_engine.REPO_ROOT = str(elsewhere)
            try:
                expect_raises(
                    (probe_claim.ClaimAuditFailed, probe_census.DocsWorktreeMissing,
                     probe_claim.ClaimError),
                    lambda: probe_claim.run_claimed_measurement(
                        "alpha", 2, root=root,
                        measure=lambda *a, **k: fake_measurement(),
                        repo_root=str(elsewhere)),
                    "an unreachable docs-wip census refuses the measurement too")
            finally:
                probe_engine.REPO_ROOT = saved
            expect(probe_claim.read_claim("alpha", root=root) is None,
                   "...leaving no claim behind")


def test_orchestration_harness_error_is_still_ingested() -> None:
    """A harness error is ingested while the claim is held, then released."""
    print("\n-- a harness error still ends in a durable record --")
    with registry(), scratch_repo() as (main_wt, _other, census):
        seeded_census(census)
        with claim_root() as root:
            outcome = probe_claim.run_claimed_measurement(
                "alpha", 2, root=root, census_path=census,
                measure=lambda p, r, **k: fake_measurement(p, r,
                                                           harness_error=True),
                repo_root=str(main_wt), renew_interval=0.05)
            expect(probe_claim.read_claim("alpha", root=root) is None
                   and outcome.claim.released,
                   "the claim is released once the harness error is durable")
        expect(outcome.exit_code == probe_claim.EXIT_HARNESS_ERROR
               and outcome.outcome == "harness-error",
               "the harness error is reported as its own outcome")
        row = probe_census.find_entry(
            json.loads(census.read_text(encoding="utf-8")), "alpha")["census"]
        expect(len(row["attempts"]) == 1 and row["attempts"][0]["accepted"] is False
               and row["current"] is None,
               "the non-accepted attempt is durably recorded, with no sample")
        expect(len(row["claims"]) == 1,
               "and the acquisition it belongs to is recorded exactly once")


def test_orchestration_refuses_a_lease_that_cannot_survive_a_run() -> None:
    """A lease shorter than one run is refused, not quietly accepted.

    This is the exact reachability a review found: `--lease-seconds 0.1`
    is a perfectly valid float, and with it the claim lapses before the
    first renewal, so a second agent can take the probe while the first
    is still measuring it. The floor is what stops that happening at
    all; `test_orchestration_aborts_when_the_claim_is_lost` covers the
    backstop for whatever a floor cannot foresee.
    """
    print("\n-- a lease that cannot survive one run is refused --")
    with registry(), scratch_repo() as (main_wt, _other, census):
        seeded_census(census)
        with claim_root() as root:
            ran = []
            for lease in (0.1, 1.0, 30.0,
                          probe_claim.MIN_ORCHESTRATION_LEASE_SECONDS - 1):
                expect_raises(
                    probe_claim.ClaimError,
                    lambda s=lease: probe_claim.run_claimed_measurement(
                        "alpha", 2, root=root, census_path=census,
                        lease_seconds=s,
                        measure=lambda *a, **k: ran.append(1) or fake_measurement(),
                        repo_root=str(main_wt)),
                    f"a {lease}s lease is refused with the floor named",
                    str(int(probe_claim.MIN_ORCHESTRATION_LEASE_SECONDS)))
                expect(probe_claim.read_claim("alpha", root=root) is None,
                       f"...and a {lease}s lease claimed nothing")
            expect(not ran, "no refused lease ever executed the probe")
            expect(probe_claim.MIN_ORCHESTRATION_LEASE_SECONDS
                   >= 2 * probe_runner_registry.DEFAULT_TIMEOUT
                   and probe_claim.LEASE_SECONDS
                   >= probe_claim.MIN_ORCHESTRATION_LEASE_SECONDS,
                   "the floor covers a full-timeout run and the default "
                   "clears the floor")

        # The CLI is the surface the review reached this through, and
        # `float` parses more than numbers: `nan` fails every ordering
        # comparison and `inf` passes every lower bound, so both slip
        # past a bare `<` and reach `timedelta`, which raises. Each must
        # meet a controlled refusal, never a traceback.
        import io
        from contextlib import redirect_stdout, redirect_stderr

        def cli_lease(text):
            out, err = io.StringIO(), io.StringIO()
            try:
                with redirect_stdout(out), redirect_stderr(err):
                    code = probe_claim.main(["--probe", "alpha", "--runs", "2",
                                             "--lease-seconds", text])
            except SystemExit as exit_code:
                code = exit_code.code if isinstance(exit_code.code, int) else 1
            except BaseException as error:  # noqa: BLE001
                return None, f"{type(error).__name__}: {error}"
            return code, err.getvalue()

        code, err = cli_lease("0.1")
        expect(code == probe_claim.EXIT_REJECTED and "--lease-seconds" in err,
               "`--lease-seconds 0.1` is rejected by the CLI too")
        # A lease can also be finite, positive and still unusable: big
        # enough and `timedelta` overflows, which is the same traceback
        # in a different disguise. The bound is where the census's own
        # cap sits, so a lease that is accepted is one the acquisition
        # record can hold too.
        for text in ("nan", "inf", "-inf", "infinity", "-1", "1e100",
                     "1e300", str(probe_claim.MAX_LEASE_SECONDS + 1)):
            code, err = cli_lease(text)
            expect(code == probe_claim.EXIT_REJECTED,
                   f"`--lease-seconds {text}` is a controlled refusal, not a "
                   f"traceback (got {code!r}: {err.strip()[:120]})")
        for text in ("nan", "inf", "infinity"):
            _code, err = cli_lease(text)
            expect("finite" in err,
                   f"...and `--lease-seconds {text}` says it must be finite")
        for text in ("1e100", "1e300"):
            _code, err = cli_lease(text)
            expect(str(int(probe_claim.MAX_LEASE_SECONDS)) in err,
                   f"...and `--lease-seconds {text}` names the maximum")

        # The same values through the low-level API, which #1436 and any
        # other caller reach directly.
        with claim_root() as root:
            for value in (float("nan"), float("inf"), float("-inf"), 0, -1,
                          True, None, "600", 1e100, 1e300,
                          probe_claim.MAX_LEASE_SECONDS + 1):
                expect_raises(
                    probe_claim.ClaimError,
                    lambda v=value: probe_claim.acquire("alpha", root=root,
                                                        lease_seconds=v),
                    f"acquire refuses a {value!r} lease",
                    "lease")
                expect(probe_claim.read_claim("alpha", root=root) is None,
                       f"...and a {value!r} lease claimed nothing")
            # The bound is inclusive, and a lease at it really works —
            # a refusal that also rejected every usable value would be
            # a different bug wearing this one's clothes.
            edge = probe_claim.acquire(
                "gamma", root=root,
                lease_seconds=probe_claim.MAX_LEASE_SECONDS)
            expect(probe_claim.read_claim("gamma", root=root)["token"]
                   == edge.token,
                   f"a lease of exactly {probe_claim.MAX_LEASE_SECONDS:.0f} "
                   f"is accepted and really claims the probe")
            # The bound and the census's own cap must not drift: a lease
            # this accepts has to be one the acquisition RECORD can hold,
            # or the refusal simply moves to after the probe is claimed.
            declared = probe_census.load_schema()["$defs"]["claim"][
                "properties"]["lease_seconds"]["maximum"]
            expect(declared == probe_claim.MAX_LEASE_SECONDS,
                   f"the lease bound is exactly the census `claim` schema's "
                   f"own cap (tool {probe_claim.MAX_LEASE_SECONDS!r}, schema "
                   f"{declared!r})")
            expect_raises(
                probe_claim.ClaimError,
                lambda: probe_claim.Renewer(
                    probe_claim.Claim("alpha", "t", root / "x", root, {}, 600),
                    interval=float("nan")),
                "and a non-finite renewal interval is refused rather than "
                "spun on", "finite")


def test_a_short_lease_means_what_it_says() -> None:
    """Sub-second leases are not rounded away by the stored timestamps.

    Whole-second timestamps used to round a lease DOWN, so a 0.1s lease
    was born already expired and a 60s one could lapse a second early.
    Nothing may depend on the rounding being generous, so the claim file
    keeps microseconds — while still reading a second-precision file a
    previous build wrote.
    """
    print("\n-- a short lease is not rounded away --")
    with registry(), claim_root() as root:
        claim = probe_claim.acquire("alpha", root=root, lease_seconds=0.5)
        stored = probe_claim.read_claim("alpha", root=root)
        expect("." in stored["expires_at"],
               "the stored expiry carries sub-second precision")
        acquired = probe_claim.parse_stamp(stored["acquired_at"])
        expires = probe_claim.parse_stamp(stored["expires_at"])
        expect(abs((expires - acquired).total_seconds() - 0.5) < 0.001,
               "the stored lease is exactly the one that was asked for")
        expect(probe_claim.utc_now() < expires,
               "and a fresh sub-second claim is not already expired")
        expect_raises(probe_claim.ClaimDenied,
                      lambda: probe_claim.acquire("alpha", root=root,
                                                  lease_seconds=0.5),
                      "so a competitor is still denied immediately after it")
        time.sleep(0.6)
        taken = probe_claim.acquire("alpha", root=root, lease_seconds=0.5)
        expect(taken.token != claim.token,
               "and it really does lapse once that half-second is gone")

        # A second-precision claim from an older build still reads, and
        # its timestamps still agree with its lease to within the
        # rounding that precision carries.
        moment = probe_claim.utc_now()
        legacy = {**stored, "probe": "beta", "token": "legacy-token",
                  "lease_seconds": 600.0,
                  "acquired_at": probe_claim.stamp_second(moment),
                  "renewed_at": probe_claim.stamp_second(moment),
                  "expires_at": probe_claim.stamp_second(
                      moment + timedelta(seconds=600))}
        probe_claim.claim_path("beta", root).write_text(
            json.dumps(legacy), encoding="utf-8")
        expect_raises(probe_claim.ClaimDenied,
                      lambda: probe_claim.acquire("beta", root=root,
                                                  lease_seconds=600),
                      "a second-precision claim file is still honoured")
        expect(probe_claim.read_claim("beta", root=root) is not None,
               "...as a live claim, not aged out as malformed: the "
               "consistency check tolerates exactly that rounding")

        # The renewer's own cadence must sit INSIDE the lease it renews.
        for lease in (0.3, 2.0, 600.0, probe_claim.LEASE_SECONDS):
            probe = probe_claim.Claim("alpha", "t", root / "x", root, {}, lease)
            interval = probe_claim.Renewer(probe).interval
            expect(interval < lease,
                   f"a {lease}s lease renews every {interval}s, inside it")


def test_orchestration_aborts_when_the_claim_is_lost() -> None:
    """A measurement whose claim was taken over ingests NOTHING.

    The claim is what makes a measurement an EXCLUSIVE observation. If
    it was lost while the probe ran, a second agent may have been
    measuring the same probe at the same time, and neither result is the
    thing the census is a record of — so the result is refused rather
    than recorded, and the artifacts are kept for whoever investigates.
    """
    print("\n-- a lost claim refuses to ingest its own result --")
    with registry(), scratch_repo() as (main_wt, _other, census):
        seeded_census(census)
        with claim_root() as root:
            thief = {}

            def measure(probe, runs, **kwargs):
                # Exactly what a stalled agent meets: its lease elapsed
                # and somebody legitimately reclaimed the probe.
                thief["claim"] = probe_claim.acquire(
                    probe, root=root,
                    lease_seconds=probe_claim.LEASE_SECONDS,
                    now=probe_claim.utc_now()
                    + timedelta(seconds=2 * probe_claim.LEASE_SECONDS))
                return fake_measurement(probe, runs)

            before = census.read_bytes()
            expect_raises(
                probe_claim.ClaimLostDuringRun,
                lambda: probe_claim.run_claimed_measurement(
                    "alpha", 2, root=root, census_path=census, measure=measure,
                    repo_root=str(main_wt), renew_interval=3600),
                "a measurement that lost its claim refuses to ingest",
                "lost while the probe was running", "nothing was recorded")

            document = json.loads(census.read_text(encoding="utf-8"))
            row = probe_census.find_entry(document, "alpha")["census"]
            expect(row["attempts"] == [] and row["current"] is None,
                   "no sample and no attempt reached the census")
            expect(len(row["claims"]) == 1,
                   "only the acquisition itself was ever recorded")
            expect(census.read_bytes() != before,
                   "...which is the one write this run made")
            survivor = probe_claim.read_claim("alpha", root=root)
            expect(survivor is not None
                   and survivor["token"] == thief["claim"].token,
                   "and the successor's claim survived the loser's unwind")

            # The same through the CLI, for its distinct exit code.
            thief["claim"].release()
            import io
            from contextlib import redirect_stdout, redirect_stderr
            out, err = io.StringIO(), io.StringIO()
            saved = probe_claim.probe_flake.measure
            probe_claim.probe_flake.measure = measure
            saved_root = probe_claim.repository_claim_root
            probe_claim.repository_claim_root = lambda *a, **k: root
            try:
                with redirect_stdout(out), redirect_stderr(err):
                    code = probe_claim.main(["--probe", "alpha", "--runs", "2"])
            finally:
                probe_claim.probe_flake.measure = saved
                probe_claim.repository_claim_root = saved_root
            expect(code == probe_claim.EXIT_CLAIM_LOST,
                   "the CLI reports its own exit code for a lost claim")
            expect("lost while the probe was running" in err.getvalue(),
                   "...and says so")


def test_orchestration_rejects_before_claiming() -> None:
    """An unmeasurable probe never takes a claim off anyone's list."""
    print("\n-- an unmeasurable probe is rejected before claiming --")
    with registry(ci_eligible={"beta"},
                 protocol={"alpha": probe_protocol.PROTOCOL_VERSION}), \
            scratch_repo() as (main_wt, _other, census):
        seeded_census(census)
        with claim_root() as root:
            for probe, why in (("beta", "CI-eligible"), ("gamma", "legacy")):
                expect_raises(
                    probe_flake.Rejection,
                    lambda p=probe: probe_claim.run_claimed_measurement(
                        p, 2, root=root, census_path=census,
                        measure=lambda *a, **k: fake_measurement(),
                        repo_root=str(main_wt)),
                    f"a {why} probe is rejected before anything is claimed")
                expect(probe_claim.read_claim(probe, root=root) is None,
                       f"...and no claim exists for the {why} probe")
            expect_raises(
                probe_claim.ClaimError,
                lambda: probe_claim.run_claimed_measurement(
                    "alpha", 0, root=root, census_path=census,
                    measure=lambda *a, **k: fake_measurement(),
                    repo_root=str(main_wt)),
                "a non-positive run count is refused", "positive")


def test_ingestion_cannot_be_overtaken() -> None:
    """No acquisition can interleave with the result being ingested.

    Checking ownership and then writing are two steps, and a review
    found the gap between them: the renewer has stopped by then, so a
    slow census commit can outlive the lease, another agent can acquire
    the probe and start measuring it, and this process publishes anyway
    on the strength of an answer that is no longer true.

    `commit_while_held` closes it by doing both inside ONE hold of the
    sidecar lock. This case proves that with a real second process: a
    contender whose own clock is far enough ahead that the claim looks
    lapsed to it, released onto the lock at the exact moment ingestion
    begins. It must still be waiting when the commit finishes.
    """
    print("\n-- ingestion cannot be overtaken mid-write --")
    with registry(), scratch_repo() as (main_wt, _other, census):
        seeded_census(census)
        with scratch() as base:
            root = base / "probe-claims"
            root.mkdir(parents=True)
            barrier = base / "go"
            observed = {}

            def ingest_slowly(target, document):
                # The contender is started HERE, inside the commit, so
                # its whole life overlaps the window under test.
                contender = subprocess.Popen(
                    [sys.executable, "-c", CONTENDER, str(root), "alpha",
                     str(barrier), "600",
                     "0", str(4 * probe_claim.LEASE_SECONDS)],
                    stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
                observed["contender"] = contender
                barrier.write_text("go", encoding="utf-8")
                ready = Path(str(barrier) + ".ready")
                deadline = time.time() + 60
                while time.time() < deadline and not ready.exists():
                    time.sleep(0.01)
                observed["reached_the_lock"] = ready.exists()
                # It has reached the lock; give it every chance to win.
                time.sleep(0.5)
                observed["still_waiting"] = contender.poll() is None
                observed["owner_at_commit"] = probe_claim.read_claim(
                    "alpha", root=root)
                return probe_census.record_result(target, document)

            outcome = probe_claim.run_claimed_measurement(
                "alpha", 2, root=root, census_path=census,
                measure=lambda p, r, **k: fake_measurement(p, r),
                repo_root=str(main_wt), renew_interval=3600,
                record_result=ingest_slowly)

            contender = observed["contender"]
            out, _err = contender.communicate(timeout=120)

        expect(observed.get("reached_the_lock") is True,
               "the contender really did reach the claim lock, so the "
               "assertion below is not vacuous")
        expect(observed.get("still_waiting") is True,
               "a competing acquisition CANNOT proceed while the result is "
               "being ingested — it blocks on the same lock")
        expect(observed["owner_at_commit"] is not None
               and observed["owner_at_commit"]["token"] == outcome.claim.token,
               "so the claim is still ours at the instant of the commit")
        expect(outcome.exit_code == probe_claim.EXIT_OK,
               "and the measurement it protected was ingested")
        row = probe_census.find_entry(
            json.loads(census.read_text(encoding="utf-8")), "alpha")["census"]
        expect(len(row["attempts"]) == 1 and row["current"] is not None,
               "...exactly once")
        expect(json.loads(out.strip().splitlines()[-1])["outcome"] == "won",
               "the contender took the probe only afterwards, once the "
               "claim was released")


def test_commit_while_held_renews_and_refuses() -> None:
    """The commit hold renews the lease, and a lost claim never commits."""
    print("\n-- commit_while_held: renew, then commit, or refuse --")
    with registry(), claim_root() as root:
        claim = probe_claim.acquire("alpha", root=root, lease_seconds=600)
        before = probe_claim.read_claim("alpha", root=root)["expires_at"]
        committed = []
        expect(claim.commit_while_held(
            lambda: committed.append("done") or "value",
            now=probe_claim.utc_now() + timedelta(seconds=300)) == "value",
            "a held claim runs the commit and returns its value")
        after = probe_claim.read_claim("alpha", root=root)["expires_at"]
        expect(committed == ["done"], "the commit ran exactly once")
        expect(after > before,
               "and the lease was renewed inside the hold, so it cannot "
               "elapse however long the commit takes")

        lapsed = probe_claim.acquire("beta", root=root, lease_seconds=1)
        probe_claim.acquire("beta", root=root, lease_seconds=600,
                            now=probe_claim.utc_now() + timedelta(seconds=60))
        ran = []
        expect_raises(probe_claim.ClaimLost,
                      lambda: lapsed.commit_while_held(
                          lambda: ran.append(1)),
                      "a claim taken over refuses to commit at all",
                      "no longer ours")
        expect(not ran, "...and the commit never ran")


def test_a_delayed_audit_cannot_be_overtaken() -> None:
    """The acquisition audit is a census write, and it is held too.

    A review found this: recording the acquisition happens BEFORE the
    renewer starts, and it is a census mutation, so it can block on
    another writer's lock for as long as that writer takes. A lease that
    elapsed meanwhile would let a second agent take the probe — and this
    process would still go on to run it, which is exactly the duplicate
    measurement the claim exists to prevent.

    Two halves, both proved here: no acquisition can interleave with the
    audit, and if ownership is gone by the time the probe would start,
    the probe does not start.
    """
    print("\n-- a delayed acquisition audit cannot be overtaken --")
    with registry(), scratch_repo() as (main_wt, _other, census):
        seeded_census(census)

        # Half one: a real contender, released onto the lock at the
        # instant the audit begins, must still be waiting when it ends.
        with scratch() as base:
            root = base / "probe-claims"
            root.mkdir(parents=True)
            barrier = base / "go"
            observed = {}

            def audit_slowly(target, probe, record):
                # This contender reads the REAL clock, so what it finds
                # once it finally gets the lock is the claim as it truly
                # stands: freshly renewed inside the hold, and therefore
                # live. It must be denied, not merely delayed.
                contender = subprocess.Popen(
                    [sys.executable, "-c", CONTENDER, str(root), "alpha",
                     str(barrier), "600", "0", "0"],
                    stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
                observed["contender"] = contender
                barrier.write_text("go", encoding="utf-8")
                ready = Path(str(barrier) + ".ready")
                deadline = time.time() + 60
                while time.time() < deadline and not ready.exists():
                    time.sleep(0.01)
                observed["reached_the_lock"] = ready.exists()
                time.sleep(0.5)
                observed["still_waiting"] = contender.poll() is None
                return probe_census.record_claim(target, probe, record)

            ran = []
            outcome = probe_claim.run_claimed_measurement(
                "alpha", 2, root=root, census_path=census,
                measure=lambda p, r, **k: ran.append(1) or fake_measurement(p, r),
                record_claim=audit_slowly, repo_root=str(main_wt),
                renew_interval=3600)
            out, _err = observed["contender"].communicate(timeout=120)
            observed["contender_outcome"] = out.strip().splitlines()[-1]

        expect(observed.get("reached_the_lock") is True,
               "the contender really did reach the claim lock during the "
               "audit, so the assertion below is not vacuous")
        expect(observed.get("still_waiting") is True,
               "a competing acquisition CANNOT proceed while the acquisition "
               "is being recorded")
        expect(ran == [1] and outcome.exit_code == probe_claim.EXIT_OK,
               "and the probe this run still owned did run")
        expect(json.loads(observed["contender_outcome"])["outcome"] == "denied",
               "the contender was DENIED once it got the lock, because the "
               "lease was renewed inside the hold rather than left to elapse "
               "for however long the audit took")

        # Half two: ownership genuinely gone by the time the probe would
        # start. The steal writes the claim file directly, because a
        # legitimate acquisition cannot get in at all any more — which is
        # the point of half one, and would make this half untestable
        # through `acquire`.
        with claim_root() as root:
            before = census.read_bytes()
            ran = []

            def audit_then_lose(target, probe, record):
                result = probe_census.record_claim(target, probe, record)
                stolen = dict(probe_claim.read_claim(probe, root=root))
                stolen["token"] = "somebody-else"
                probe_claim.claim_path(probe, root).write_text(
                    json.dumps(stolen), encoding="utf-8")
                return result

            expect_raises(
                probe_claim.ClaimLostDuringRun,
                lambda: probe_claim.run_claimed_measurement(
                    "beta", 2, root=root, census_path=census,
                    measure=lambda *a, **k: ran.append(1) or fake_measurement("beta"),
                    record_claim=audit_then_lose, repo_root=str(main_wt),
                    renew_interval=3600),
                "a claim lost before the measurement refuses to start it",
                "before the measurement started", "was not run")
            expect(not ran,
                   "the probe was NEVER run — which is the duplicate "
                   "measurement this whole feature exists to prevent")
            row = probe_census.find_entry(
                json.loads(census.read_text(encoding="utf-8")),
                "beta")["census"]
            expect(row["attempts"] == [] and row["current"] is None,
                   "and no result reached the census")
            expect(census.read_bytes() != before and len(row["claims"]) == 1,
                   "only the acquisition the audit itself wrote is recorded")
            expect(probe_claim.read_claim("beta", root=root)["token"]
                   == "somebody-else",
                   "and the new owner's claim was left alone on the way out")


def test_a_completed_measurement_is_never_lost() -> None:
    """An ingestion failure must not cost an hour of engine time.

    A review found this: the result document used to be written by the
    CLI AFTER `run_claimed_measurement` returned, so a census that
    refused the measurement unwound past it — and `probe_flake` has
    already pruned the artifact directories of every successful run, so
    there was nothing left to re-ingest once the census was fixed. The
    document is now written the moment the measurement exists, before
    anything that can fail.

    The proof is recovery, not existence: the retained file is ingested
    into a working census afterwards and produces the sample the failed
    run would have.
    """
    print("\n-- a completed measurement survives a failed ingestion --")

    def refusing(target, document):
        raise probe_census.CensusError("injected: the census refused it")

    # (a) With an explicit --result path.
    with registry(), scratch_repo() as (main_wt, _other, census):
        seeded_census(census)
        with claim_root() as root, scratch() as out:
            wanted = out / "nested" / "role.json"
            error = None
            try:
                probe_claim.run_claimed_measurement(
                    "alpha", 2, root=root, census_path=census,
                    result_path=wanted,
                    measure=lambda p, r, **k: fake_measurement(p, r),
                    record_result=refusing, repo_root=str(main_wt),
                    renew_interval=3600)
            except probe_claim.ResultIngestionFailed as raised:
                error = raised
            expect(error is not None,
                   "a census that refuses the measurement is its own failure")
            expect(wanted.exists(),
                   "...and the requested result document was written anyway")
            expect(str(wanted) in str(error)
                   and "--record" in str(error),
                   "...with the diagnostic naming it and how to re-ingest it")
            expect(error.retained == wanted,
                   "...and the exception carrying the retained path")
            row = probe_census.find_entry(
                json.loads(census.read_text(encoding="utf-8")),
                "alpha")["census"]
            expect(row["attempts"] == [] and row["current"] is None
                   and len(row["claims"]) == 1,
                   "the census really did refuse the measurement, holding "
                   "only the acquisition record")

            # Recovery: the retained document ingests once the cause is
            # fixed. That is what makes retention worth anything — and
            # it is reported rather than raised when the document is
            # missing, so a regression says so instead of aborting the
            # cases below it.
            try:
                document = json.loads(wanted.read_text(encoding="utf-8"))
            except OSError as missing:
                document = None
                expect(False, f"the retained document is readable ({missing})")
            if document is not None:
                expect(document["schema"] == probe_census.RESULT_SCHEMA
                       and document["probe"] == "alpha",
                       "the retained file is a complete probe-flake-result/v1")
                probe_census.record_result(census, document)
                row = probe_census.find_entry(
                    json.loads(census.read_text(encoding="utf-8")),
                    "alpha")["census"]
                expect(len(row["attempts"]) == 1 and row["current"] is not None,
                       "...and re-ingesting it lands exactly the lost sample")

    # (b) With no --result at all: the run's own invocation directory.
    with registry(), scratch_repo() as (main_wt, _other, census):
        seeded_census(census)
        with claim_root() as root, scratch() as artifacts:
            made = fake_measurement("alpha", 2, artifact_root=artifacts)
            error = None
            try:
                probe_claim.run_claimed_measurement(
                    "alpha", 2, root=root, census_path=census,
                    measure=lambda *a, **k: made,
                    record_result=refusing, repo_root=str(main_wt),
                    renew_interval=3600)
            except probe_claim.ResultIngestionFailed as raised:
                error = raised
            fallback = (Path(made.invocation_dir)
                        / probe_claim.RETAINED_RESULT_NAME)
            expect(error is not None and fallback.exists(),
                   "a measurement nobody asked for a copy of is retained "
                   "beside its own artifacts")
            try:
                same = json.loads(
                    fallback.read_text(encoding="utf-8"))["probe"] == "alpha"
                detail = ""
            except OSError as missing:
                same, detail = False, f" ({missing})"
            expect(same,
                   f"...as the same complete result document{detail}")

    # (c) A lost claim keeps it too — that path ingests nothing either.
    with registry(), scratch_repo() as (main_wt, _other, census):
        seeded_census(census)
        with claim_root() as root, scratch() as out:
            wanted = out / "lost.json"

            def steal(probe, runs, **kwargs):
                probe_claim.acquire(
                    probe, root=root,
                    lease_seconds=probe_claim.LEASE_SECONDS,
                    now=probe_claim.utc_now()
                    + timedelta(seconds=2 * probe_claim.LEASE_SECONDS))
                return fake_measurement(probe, runs)

            expect_raises(
                probe_claim.ClaimLostDuringRun,
                lambda: probe_claim.run_claimed_measurement(
                    "alpha", 2, root=root, census_path=census,
                    result_path=wanted, measure=steal,
                    repo_root=str(main_wt), renew_interval=3600),
                "a lost claim still refuses to ingest", "retained at")
            expect(wanted.exists(),
                   "...but the measurement it refused is retained all the same")

    # (d) An unusable --result is refused BEFORE anything is claimed —
    # the TARGET itself, not merely its directory. Every shape here has
    # a perfectly writable parent, which is exactly why checking the
    # parent alone let the probe be claimed and run first.
    with registry(), scratch_repo() as (main_wt, _other, census):
        seeded_census(census)
        with claim_root() as root, scratch() as out:
            a_file = out / "a-file"
            a_file.write_text("not a directory", encoding="utf-8")
            a_directory = out / "a-directory"
            a_directory.mkdir()
            dangling = out / "dangling.json"
            dangling.symlink_to(out / "nothing-here")
            cases = [
                (a_file / "result.json", "a path under a plain file"),
                (a_directory, "an existing DIRECTORY at the target"),
                (dangling, "a dangling symlink"),
            ]
            # The mode-based case only exists where the mode is obeyed.
            # CI runs this container as root, and root writes a 0444
            # file regardless — so `check_result_path` accepting it is
            # CORRECT there, and asserting a refusal would be asserting
            # that the tool lies about what it can write. The condition
            # is the same question the tool itself asks, so the two
            # cannot disagree about which environment this is.
            read_only = out / "read-only.json"
            read_only.write_text("{}", encoding="utf-8")
            read_only.chmod(0o444)
            if os.access(read_only, os.W_OK):
                skip("an existing unwritable file: this process may write a "
                     "0444 file anyway (running as root), so there is no "
                     "unwritable destination to refuse")
            else:
                cases.append((read_only, "an existing unwritable file"))
            for target, why in cases:
                ran = []
                expect_raises(
                    probe_claim.ClaimError,
                    lambda p=target: probe_claim.run_claimed_measurement(
                        "alpha", 2, root=root, census_path=census,
                        result_path=p,
                        measure=lambda *a, **k: (ran.append(1)
                                                 or fake_measurement()),
                        repo_root=str(main_wt)),
                    f"{why} is refused up front, not after an hour of engine "
                    f"time", "result document cannot be written")
                expect(not ran
                       and probe_claim.read_claim("alpha", root=root) is None,
                       f"...having run nothing and claimed nothing ({why})")
            # And an ordinary writable existing file is still fine: a
            # rule that refused every target would be a different bug.
            reusable = out / "reusable.json"
            reusable.write_text("{}", encoding="utf-8")
            expect(probe_claim.check_result_path(reusable) == reusable,
                   "an existing writable file is still a usable destination")

    # (e) The happy path writes it too, and says so.
    with registry(), scratch_repo() as (main_wt, _other, census):
        seeded_census(census)
        with claim_root() as root, scratch() as out:
            wanted = out / "ok.json"
            outcome = probe_claim.run_claimed_measurement(
                "alpha", 2, root=root, census_path=census, result_path=wanted,
                measure=lambda p, r, **k: fake_measurement(p, r),
                repo_root=str(main_wt), renew_interval=3600)
            expect(outcome.exit_code == probe_claim.EXIT_OK
                   and wanted.exists() and outcome.result_path == wanted
                   and outcome.result_problem is None,
                   "a successful run writes the requested result document")
            expect(outcome.to_document()["result_document"] == str(wanted),
                   "...and reports where it is")


def test_probe_flake_needs_no_docs_worktree() -> None:
    """The low-level measurement API stays usable without a census.

    `probe_flake.py` guarantees that a fresh checkout with no `docs-wip`
    worktree behaves identically, and the census-backed claim must not
    quietly take that away: the mandatory claim belongs to the
    ORCHESTRATION path, not to the measurement API.
    """
    print("\n-- probe_flake stays usable with no docs worktree --")
    with registry(), scratch() as elsewhere:
        saved = probe_engine.REPO_ROOT
        probe_engine.REPO_ROOT = str(elsewhere)
        try:
            expect_raises(probe_census.DocsWorktreeMissing,
                          lambda: probe_census.manifest_path(),
                          "the scratch tree really has no docs-wip census")
            # Every pre-execution decision the harness makes is reachable.
            expect(probe_flake.protocol_status("alpha")
                   == probe_protocol.PROTOCOL_VERSION,
                   "probe_flake resolves protocol status with no census")
            expect(probe_flake.resolve_probe("alpha") == "alpha_probe.py",
                   "and resolves a probe to its script with no census")
            expect_raises(probe_flake.Rejection,
                          lambda: probe_flake.measure("alpha", 0),
                          "and its own argument checking still refuses first",
                          "positive")
            expect("probe_census" not in probe_flake.__dict__,
                   "probe_flake does not import the census at all")
        finally:
            probe_engine.REPO_ROOT = saved


def test_cli() -> None:
    """The CLI's outcomes, exit codes and read-only status view."""
    print("\n-- the command line --")
    import io
    from contextlib import redirect_stdout, redirect_stderr

    def cli(*argv):
        out, err = io.StringIO(), io.StringIO()
        try:
            with redirect_stdout(out), redirect_stderr(err):
                code = probe_claim.main(list(argv))
        except SystemExit as exit_code:
            code = exit_code.code if isinstance(exit_code.code, int) else 1
        return code, out.getvalue(), err.getvalue()

    with registry(), scratch_repo() as (main_wt, _other, _census):
        code, out, _err = cli("--status", "--json")
        expect(code == probe_claim.EXIT_OK and json.loads(out)["claims"] == [],
               "--status on an unclaimed repository reports no claims")

        claim = probe_claim.acquire("alpha", lease_seconds=600,
                                    repo_root=str(main_wt))
        try:
            code, out, _err = cli("--status", "--json")
            rows = json.loads(out)["claims"]
            expect(code == probe_claim.EXIT_OK and len(rows) == 1
                   and rows[0]["probe"] == "alpha" and rows[0]["state"] == "held",
                   "--status reports a held claim and its owner")

            code, _out, err = cli("--probe", "alpha", "--runs", "2")
            expect(code == probe_claim.EXIT_ALREADY_CLAIMED
                   and "already claimed" in err,
                   "a denied run exits with the already-claimed code")
        finally:
            claim.release()

        code, _out, err = cli("--probe", "nonesuch", "--runs", "2")
        expect(code == probe_claim.EXIT_REJECTED and "nonesuch" in err,
               "an unknown probe is rejected")
        code, _out, err = cli("--runs", "2")
        expect(code != probe_claim.EXIT_OK,
               "--runs without --probe is a usage error")


# ==========================================================================
def main() -> int:
    selftest.parse_verbose()
    for test in (test_namespace, test_exclusive_acquisition,
                 test_independent_process_contention,
                 test_expiry_and_reclaim, test_concurrent_stale_reclaimers,
                 test_owner_safe_late_release,
                 test_a_contended_acquisition_gets_a_live_lease,
                 test_expiry_is_one_way,
                 test_malformed_claim,
                 test_release_on_every_managed_exit,
                 test_crash_recovery_through_ttl,
                 test_renewer_keeps_a_long_measurement_alive,
                 test_census_claim_collection, test_claims_are_not_measurements,
                 test_schema_migration_is_lossless,
                 test_orchestration_happy_path,
                 test_orchestration_denied_creates_nothing,
                 test_orchestration_claim_audit_failure,
                 test_orchestration_harness_error_is_still_ingested,
                 test_orchestration_rejects_before_claiming,
                 test_orchestration_refuses_a_lease_that_cannot_survive_a_run,
                 test_a_short_lease_means_what_it_says,
                 test_orchestration_aborts_when_the_claim_is_lost,
                 test_ingestion_cannot_be_overtaken,
                 test_a_delayed_audit_cannot_be_overtaken,
                 test_commit_while_held_renews_and_refuses,
                 test_a_completed_measurement_is_never_lost,
                 test_probe_flake_needs_no_docs_worktree, test_cli):
        test()
    print()
    if FAILURES:
        print(f"{len(FAILURES)} FAILED:")
        for message in FAILURES:
            print(f"  - {message}")
        return selftest.concluded(1)
    return selftest.concluded(0, "probe_claim self-test: all cases pass")


if __name__ == "__main__":
    sys.exit(main())
