#!/usr/bin/env python3
"""Claimed-measurement orchestration cases (#1434), split out by #2100.

The thirteen cases covering the successful and denied measurements, the
acquisition-audit failure, harness-error ingestion, pre-claim rejection,
lease validation, lost-claim handling, serialized audit and ingestion,
retained results and their `--result` destination validation, and the
CLI's outcomes and exit codes.

Not a gate of its own. Run through the aggregate:

  python3 tools/test_probe_claim.py --only orchestration
"""
from __future__ import annotations

import json
import os
import subprocess
import sys
import time
from datetime import timedelta
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import probe_census  # type: ignore  # noqa: E402
import probe_claim  # type: ignore  # noqa: E402
import probe_claim_lease as claim_lease  # type: ignore  # noqa: E402
import probe_claim_orchestration as claim_orchestration  # type: ignore  # noqa: E402
import probe_claim_storage as claim_storage  # type: ignore  # noqa: E402
import probe_engine  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
import probe_protocol  # type: ignore  # noqa: E402
import probe_runner_registry  # type: ignore  # noqa: E402
from probe_claim_selftest_support import (  # noqa: E402
    CONTENDER, claim_root, expect, expect_raises, fake_measurement, registry,
    scratch, scratch_repo, seeded_census, skip)


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
                seen["claim"] = claim_lease.read_claim(probe, root=root)
                document = json.loads(census.read_text(encoding="utf-8"))
                seen["claims"] = probe_census.find_entry(
                    document, probe)["census"]["claims"]
                seen["attempts"] = probe_census.find_entry(
                    document, probe)["census"]["attempts"]
                return fake_measurement(probe, runs)

            outcome = claim_orchestration.run_claimed_measurement(
                "alpha", 2, root=root, census_path=census, measure=measure,
                repo_root=str(main_wt), renew_interval=0.05)

        expect(outcome.exit_code == claim_orchestration.EXIT_OK
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
            holder = claim_lease.acquire("alpha", root=root, lease_seconds=600)
            before = census.read_bytes()
            ran = []
            outcome = claim_orchestration.run_claimed_measurement(
                "alpha", 2, root=root, census_path=census,
                measure=lambda *a, **k: ran.append(1) or fake_measurement(),
                repo_root=str(main_wt))
            expect(outcome.exit_code == claim_orchestration.EXIT_ALREADY_CLAIMED,
                   "the run reports the already-claimed exit code")
            expect(outcome.denied is not None
                   and outcome.denied.token == holder.token,
                   "and names the holder in its outcome")
            expect(not ran, "the probe was never executed")
            expect(census.read_bytes() == before,
                   "the census is byte-for-byte unchanged")
            expect(claim_lease.read_claim("alpha", root=root)["token"]
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
                claim_orchestration.ClaimAuditFailed,
                lambda: claim_orchestration.run_claimed_measurement(
                    "alpha", 2, root=root, census_path=census,
                    measure=lambda *a, **k: ran.append(1) or fake_measurement(),
                    repo_root=str(main_wt)),
                "an unrecordable acquisition refuses the measurement",
                "could not be recorded", "released")
            expect(not ran, "the probe was never executed")
            expect(claim_lease.read_claim("alpha", root=root) is None,
                   "and the claim was released, not left wedged")

        # The same, with the docs worktree itself unreachable.
        with claim_root() as root, scratch() as elsewhere:
            saved = probe_engine.REPO_ROOT
            probe_engine.REPO_ROOT = str(elsewhere)
            try:
                expect_raises(
                    (claim_orchestration.ClaimAuditFailed,
                     probe_census.DocsWorktreeMissing,
                     claim_storage.ClaimError),
                    lambda: claim_orchestration.run_claimed_measurement(
                        "alpha", 2, root=root,
                        measure=lambda *a, **k: fake_measurement(),
                        repo_root=str(elsewhere)),
                    "an unreachable docs-wip census refuses the measurement too")
            finally:
                probe_engine.REPO_ROOT = saved
            expect(claim_lease.read_claim("alpha", root=root) is None,
                   "...leaving no claim behind")


def test_orchestration_harness_error_is_still_ingested() -> None:
    """A harness error is ingested while the claim is held, then released."""
    print("\n-- a harness error still ends in a durable record --")
    with registry(), scratch_repo() as (main_wt, _other, census):
        seeded_census(census)
        with claim_root() as root:
            outcome = claim_orchestration.run_claimed_measurement(
                "alpha", 2, root=root, census_path=census,
                measure=lambda p, r, **k: fake_measurement(p, r,
                                                           harness_error=True),
                repo_root=str(main_wt), renew_interval=0.05)
            expect(claim_lease.read_claim("alpha", root=root) is None
                   and outcome.claim.released,
                   "the claim is released once the harness error is durable")
        expect(outcome.exit_code == claim_orchestration.EXIT_HARNESS_ERROR
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
                          claim_orchestration.MIN_ORCHESTRATION_LEASE_SECONDS - 1):
                expect_raises(
                    claim_storage.ClaimError,
                    lambda s=lease: claim_orchestration.run_claimed_measurement(
                        "alpha", 2, root=root, census_path=census,
                        lease_seconds=s,
                        measure=lambda *a, **k: ran.append(1) or fake_measurement(),
                        repo_root=str(main_wt)),
                    f"a {lease}s lease is refused with the floor named",
                    str(int(claim_orchestration.MIN_ORCHESTRATION_LEASE_SECONDS)))
                expect(claim_lease.read_claim("alpha", root=root) is None,
                       f"...and a {lease}s lease claimed nothing")
            expect(not ran, "no refused lease ever executed the probe")
            expect(claim_orchestration.MIN_ORCHESTRATION_LEASE_SECONDS
                   >= 2 * probe_runner_registry.DEFAULT_TIMEOUT
                   and claim_lease.LEASE_SECONDS
                   >= claim_orchestration.MIN_ORCHESTRATION_LEASE_SECONDS,
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
        expect(code == claim_orchestration.EXIT_REJECTED
               and "--lease-seconds" in err,
               "`--lease-seconds 0.1` is rejected by the CLI too")
        # A lease can also be finite, positive and still unusable: big
        # enough and `timedelta` overflows, which is the same traceback
        # in a different disguise. The bound is where the census's own
        # cap sits, so a lease that is accepted is one the acquisition
        # record can hold too.
        for text in ("nan", "inf", "-inf", "infinity", "-1", "1e100",
                     "1e300", str(claim_storage.MAX_LEASE_SECONDS + 1)):
            code, err = cli_lease(text)
            expect(code == claim_orchestration.EXIT_REJECTED,
                   f"`--lease-seconds {text}` is a controlled refusal, not a "
                   f"traceback (got {code!r}: {err.strip()[:120]})")
        for text in ("nan", "inf", "infinity"):
            _code, err = cli_lease(text)
            expect("finite" in err,
                   f"...and `--lease-seconds {text}` says it must be finite")
        for text in ("1e100", "1e300"):
            _code, err = cli_lease(text)
            expect(str(int(claim_storage.MAX_LEASE_SECONDS)) in err,
                   f"...and `--lease-seconds {text}` names the maximum")

        # The same values through the low-level API, which #1436 and any
        # other caller reach directly.
        with claim_root() as root:
            for value in (float("nan"), float("inf"), float("-inf"), 0, -1,
                          True, None, "600", 1e100, 1e300,
                          claim_storage.MAX_LEASE_SECONDS + 1):
                expect_raises(
                    claim_storage.ClaimError,
                    lambda v=value: claim_lease.acquire("alpha", root=root,
                                                        lease_seconds=v),
                    f"acquire refuses a {value!r} lease",
                    "lease")
                expect(claim_lease.read_claim("alpha", root=root) is None,
                       f"...and a {value!r} lease claimed nothing")
            # The bound is inclusive, and a lease at it really works —
            # a refusal that also rejected every usable value would be
            # a different bug wearing this one's clothes.
            edge = claim_lease.acquire(
                "gamma", root=root,
                lease_seconds=claim_storage.MAX_LEASE_SECONDS)
            expect(claim_lease.read_claim("gamma", root=root)["token"]
                   == edge.token,
                   f"a lease of exactly {claim_storage.MAX_LEASE_SECONDS:.0f} "
                   f"is accepted and really claims the probe")
            # The bound and the census's own cap must not drift: a lease
            # this accepts has to be one the acquisition RECORD can hold,
            # or the refusal simply moves to after the probe is claimed.
            declared = probe_census.load_schema()["$defs"]["claim"][
                "properties"]["lease_seconds"]["maximum"]
            expect(declared == claim_storage.MAX_LEASE_SECONDS,
                   f"the lease bound is exactly the census `claim` schema's "
                   f"own cap (tool {claim_storage.MAX_LEASE_SECONDS!r}, schema "
                   f"{declared!r})")
            expect_raises(
                claim_storage.ClaimError,
                lambda: claim_lease.Renewer(
                    claim_lease.Claim("alpha", "t", root / "x", root, {}, 600),
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
        claim = claim_lease.acquire("alpha", root=root, lease_seconds=0.5)
        stored = claim_lease.read_claim("alpha", root=root)
        expect("." in stored["expires_at"],
               "the stored expiry carries sub-second precision")
        acquired = claim_storage.parse_stamp(stored["acquired_at"])
        expires = claim_storage.parse_stamp(stored["expires_at"])
        expect(abs((expires - acquired).total_seconds() - 0.5) < 0.001,
               "the stored lease is exactly the one that was asked for")
        expect(claim_storage.utc_now() < expires,
               "and a fresh sub-second claim is not already expired")
        expect_raises(claim_lease.ClaimDenied,
                      lambda: claim_lease.acquire("alpha", root=root,
                                                  lease_seconds=0.5),
                      "so a competitor is still denied immediately after it")
        time.sleep(0.6)
        taken = claim_lease.acquire("alpha", root=root, lease_seconds=0.5)
        expect(taken.token != claim.token,
               "and it really does lapse once that half-second is gone")

        # A second-precision claim from an older build still reads, and
        # its timestamps still agree with its lease to within the
        # rounding that precision carries.
        moment = claim_storage.utc_now()
        legacy = {**stored, "probe": "beta", "token": "legacy-token",
                  "lease_seconds": 600.0,
                  "acquired_at": claim_storage.stamp_second(moment),
                  "renewed_at": claim_storage.stamp_second(moment),
                  "expires_at": claim_storage.stamp_second(
                      moment + timedelta(seconds=600))}
        claim_storage.claim_path("beta", root).write_text(
            json.dumps(legacy), encoding="utf-8")
        expect_raises(claim_lease.ClaimDenied,
                      lambda: claim_lease.acquire("beta", root=root,
                                                  lease_seconds=600),
                      "a second-precision claim file is still honoured")
        expect(claim_lease.read_claim("beta", root=root) is not None,
               "...as a live claim, not aged out as malformed: the "
               "consistency check tolerates exactly that rounding")

        # The renewer's own cadence must sit INSIDE the lease it renews.
        for lease in (0.3, 2.0, 600.0, claim_lease.LEASE_SECONDS):
            probe = claim_lease.Claim("alpha", "t", root / "x", root, {}, lease)
            interval = claim_lease.Renewer(probe).interval
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
                thief["claim"] = claim_lease.acquire(
                    probe, root=root,
                    lease_seconds=claim_lease.LEASE_SECONDS,
                    now=claim_storage.utc_now()
                    + timedelta(seconds=2 * claim_lease.LEASE_SECONDS))
                return fake_measurement(probe, runs)

            before = census.read_bytes()
            expect_raises(
                claim_orchestration.ClaimLostDuringRun,
                lambda: claim_orchestration.run_claimed_measurement(
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
            survivor = claim_lease.read_claim("alpha", root=root)
            expect(survivor is not None
                   and survivor["token"] == thief["claim"].token,
                   "and the successor's claim survived the loser's unwind")

            # The same through the CLI, for its distinct exit code.
            thief["claim"].release()
            import io
            from contextlib import redirect_stdout, redirect_stderr
            out, err = io.StringIO(), io.StringIO()
            # Both seams are patched on the IMPLEMENTATION owner, not on
            # the command: `claim_orchestration` is what calls
            # `probe_flake.measure`, and `claim_storage` is what the
            # lease owner and the command's own status query resolve
            # `repository_claim_root` through, at call time (#2148).
            saved = claim_orchestration.probe_flake.measure
            claim_orchestration.probe_flake.measure = measure
            saved_root = claim_storage.repository_claim_root
            claim_storage.repository_claim_root = lambda *a, **k: root
            try:
                with redirect_stdout(out), redirect_stderr(err):
                    code = probe_claim.main(["--probe", "alpha", "--runs", "2"])
            finally:
                claim_orchestration.probe_flake.measure = saved
                claim_storage.repository_claim_root = saved_root
            expect(code == claim_orchestration.EXIT_CLAIM_LOST,
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
                    lambda p=probe: claim_orchestration.run_claimed_measurement(
                        p, 2, root=root, census_path=census,
                        measure=lambda *a, **k: fake_measurement(),
                        repo_root=str(main_wt)),
                    f"a {why} probe is rejected before anything is claimed")
                expect(claim_lease.read_claim(probe, root=root) is None,
                       f"...and no claim exists for the {why} probe")
            expect_raises(
                claim_storage.ClaimError,
                lambda: claim_orchestration.run_claimed_measurement(
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
                     "0", str(4 * claim_lease.LEASE_SECONDS)],
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
                observed["owner_at_commit"] = claim_lease.read_claim(
                    "alpha", root=root)
                return probe_census.record_result(target, document)

            outcome = claim_orchestration.run_claimed_measurement(
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
        expect(outcome.exit_code == claim_orchestration.EXIT_OK,
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
        claim = claim_lease.acquire("alpha", root=root, lease_seconds=600)
        before = claim_lease.read_claim("alpha", root=root)["expires_at"]
        committed = []
        expect(claim.commit_while_held(
            lambda: committed.append("done") or "value",
            now=claim_storage.utc_now() + timedelta(seconds=300)) == "value",
            "a held claim runs the commit and returns its value")
        after = claim_lease.read_claim("alpha", root=root)["expires_at"]
        expect(committed == ["done"], "the commit ran exactly once")
        expect(after > before,
               "and the lease was renewed inside the hold, so it cannot "
               "elapse however long the commit takes")

        lapsed = claim_lease.acquire("beta", root=root, lease_seconds=1)
        claim_lease.acquire("beta", root=root, lease_seconds=600,
                            now=claim_storage.utc_now() + timedelta(seconds=60))
        ran = []
        expect_raises(claim_lease.ClaimLost,
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
            outcome = claim_orchestration.run_claimed_measurement(
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
        expect(ran == [1] and outcome.exit_code == claim_orchestration.EXIT_OK,
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
                stolen = dict(claim_lease.read_claim(probe, root=root))
                stolen["token"] = "somebody-else"
                claim_storage.claim_path(probe, root).write_text(
                    json.dumps(stolen), encoding="utf-8")
                return result

            expect_raises(
                claim_orchestration.ClaimLostDuringRun,
                lambda: claim_orchestration.run_claimed_measurement(
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
            expect(claim_lease.read_claim("beta", root=root)["token"]
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
                claim_orchestration.run_claimed_measurement(
                    "alpha", 2, root=root, census_path=census,
                    result_path=wanted,
                    measure=lambda p, r, **k: fake_measurement(p, r),
                    record_result=refusing, repo_root=str(main_wt),
                    renew_interval=3600)
            except claim_orchestration.ResultIngestionFailed as raised:
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
                claim_orchestration.run_claimed_measurement(
                    "alpha", 2, root=root, census_path=census,
                    measure=lambda *a, **k: made,
                    record_result=refusing, repo_root=str(main_wt),
                    renew_interval=3600)
            except claim_orchestration.ResultIngestionFailed as raised:
                error = raised
            fallback = (Path(made.invocation_dir)
                        / claim_orchestration.RETAINED_RESULT_NAME)
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
                claim_lease.acquire(
                    probe, root=root,
                    lease_seconds=claim_lease.LEASE_SECONDS,
                    now=claim_storage.utc_now()
                    + timedelta(seconds=2 * claim_lease.LEASE_SECONDS))
                return fake_measurement(probe, runs)

            expect_raises(
                claim_orchestration.ClaimLostDuringRun,
                lambda: claim_orchestration.run_claimed_measurement(
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
                    claim_storage.ClaimError,
                    lambda p=target: claim_orchestration.run_claimed_measurement(
                        "alpha", 2, root=root, census_path=census,
                        result_path=p,
                        measure=lambda *a, **k: (ran.append(1)
                                                 or fake_measurement()),
                        repo_root=str(main_wt)),
                    f"{why} is refused up front, not after an hour of engine "
                    f"time", "result document cannot be written")
                expect(not ran
                       and claim_lease.read_claim("alpha", root=root) is None,
                       f"...having run nothing and claimed nothing ({why})")
            # And an ordinary writable existing file is still fine: a
            # rule that refused every target would be a different bug.
            reusable = out / "reusable.json"
            reusable.write_text("{}", encoding="utf-8")
            expect(claim_orchestration.check_result_path(reusable) == reusable,
                   "an existing writable file is still a usable destination")

    # (e) The happy path writes it too, and says so.
    with registry(), scratch_repo() as (main_wt, _other, census):
        seeded_census(census)
        with claim_root() as root, scratch() as out:
            wanted = out / "ok.json"
            outcome = claim_orchestration.run_claimed_measurement(
                "alpha", 2, root=root, census_path=census, result_path=wanted,
                measure=lambda p, r, **k: fake_measurement(p, r),
                repo_root=str(main_wt), renew_interval=3600)
            expect(outcome.exit_code == claim_orchestration.EXIT_OK
                   and wanted.exists() and outcome.result_path == wanted
                   and outcome.result_problem is None,
                   "a successful run writes the requested result document")
            expect(outcome.to_document()["result_document"] == str(wanted),
                   "...and reports where it is")


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
        expect(code == claim_orchestration.EXIT_OK
               and json.loads(out)["claims"] == [],
               "--status on an unclaimed repository reports no claims")

        claim = claim_lease.acquire("alpha", lease_seconds=600,
                                    repo_root=str(main_wt))
        try:
            code, out, _err = cli("--status", "--json")
            rows = json.loads(out)["claims"]
            expect(code == claim_orchestration.EXIT_OK and len(rows) == 1
                   and rows[0]["probe"] == "alpha" and rows[0]["state"] == "held",
                   "--status reports a held claim and its owner")

            code, _out, err = cli("--probe", "alpha", "--runs", "2")
            expect(code == claim_orchestration.EXIT_ALREADY_CLAIMED
                   and "already claimed" in err,
                   "a denied run exits with the already-claimed code")
        finally:
            claim.release()

        code, _out, err = cli("--probe", "nonesuch", "--runs", "2")
        expect(code == claim_orchestration.EXIT_REJECTED and "nonesuch" in err,
               "an unknown probe is rejected")
        code, _out, err = cli("--runs", "2")
        expect(code != claim_orchestration.EXIT_OK,
               "--runs without --probe is a usage error")


#: This owner's inventory, in the relative order these cases hold within
#: the aggregate's run sequence -- which is NOT this file's source
#: order: `test_orchestration_rejects_before_claiming` runs before
#: `test_orchestration_refuses_a_lease_that_cannot_survive_a_run`, and
#: `test_commit_while_held_renews_and_refuses` runs after
#: `test_a_delayed_audit_cannot_be_overtaken`.
CASES = (
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
    test_cli,
)
