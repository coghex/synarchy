#!/usr/bin/env python3
"""The claimed measurement: the one boundary where holding a claim pays.

The orchestration owner of `tools/probe_claim.py`'s three (#2148). It
consumes `probe_claim_lease` for ownership and `probe_claim_storage`
for the commit sha and the controlled-refusal error, and adds the two
collaborators neither of them may know about — `probe_census` and
`probe_flake`. Requiring a claim before a measurement, recording the
acquisition, keeping the completed result and refusing to publish an
unattributable one all live here.

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

The lease floor lives here, not with the lease
----------------------------------------------
`MIN_ORCHESTRATION_LEASE_SECONDS` is twice one run's timeout, and it is
enforced only at this boundary because this is the only caller that
knows a measurement is about to run. A lease that can elapse while a
single run is still going is not a short lease but a broken one — it
hands the probe to a second agent mid-measurement — so it is REFUSED
rather than silently raised. `lease.acquire` still accepts any positive
lease, which is what lets the gate drive expiry deliberately.

The exit-code table lives here too. `run_claimed_measurement` and
`Outcome` decide four of the seven themselves, and putting any of them
on the command would make this module import upward from its own
facade; the command imports the table and re-declares none of it, and
the operator-facing prose stays in the command's own docstring.

`tools/probe_flake.py` is deliberately NOT changed by any of this. Its
own contract is that the harness behaves identically on a checkout with
no docs worktree, so the census-backed claim lives here, in the
orchestration path, and the low-level measurement API stays usable on
its own.

Nothing here is a program; `tools/probe_claim.py` remains the command.
"""
from __future__ import annotations

import json
import os
import stat
import sys
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_census  # noqa: E402
import probe_claim_lease as lease  # noqa: E402
import probe_claim_storage as storage  # noqa: E402
import probe_flake  # noqa: E402
import probe_runner_registry  # noqa: E402

# The floor `run_claimed_measurement` and the CLI enforce. A lease that
# can elapse while ONE supported run is still going is not a short
# lease, it is a broken one: the probe becomes reclaimable mid-
# measurement and two agents end up measuring it at once. Twice the
# per-run timeout is the smallest value that cannot do that, and it is
# refused rather than silently raised, because a caller asking for
# thirty seconds has misunderstood what this lease is for.
MIN_ORCHESTRATION_LEASE_SECONDS = 2.0 * probe_runner_registry.DEFAULT_TIMEOUT


# Where a completed measurement is kept when the caller named no
# `--result` path of its own: beside the run's own artifacts, under the
# invocation directory `probe_flake` already created for it.
RETAINED_RESULT_NAME = "probe-flake-result.json"


EXIT_OK = 0
EXIT_REJECTED = 2
EXIT_ALREADY_CLAIMED = 3
EXIT_HARNESS_ERROR = 4
EXIT_CLAIM_AUDIT = 5
EXIT_NO_PORT = 6
EXIT_CLAIM_LOST = 7


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
            raise storage.ClaimError(
                f"the result document cannot be written to {target} "
                f"({error}); a dangling symlink is not a destination") from None
        if stat.S_ISDIR(info.st_mode):
            raise storage.ClaimError(
                f"the result document cannot be written to {target}: it is a "
                f"directory")
        if not stat.S_ISREG(info.st_mode):
            raise storage.ClaimError(
                f"the result document cannot be written to {target}: it must "
                f"be a regular file (got mode {stat.S_IFMT(info.st_mode):#o})")
        if not os.access(target, os.W_OK):
            raise storage.ClaimError(
                f"the result document cannot be written to {target}: it is "
                f"not writable")
        return target
    try:
        target.parent.mkdir(parents=True, exist_ok=True)
    except OSError as error:
        raise storage.ClaimError(
            f"the result document cannot be written to {target}: its "
            f"directory could not be created ({error})") from None
    if not target.parent.is_dir():
        raise storage.ClaimError(
            f"the result document cannot be written to {target}: "
            f"{target.parent} is not a directory")
    if not os.access(target.parent, os.W_OK | os.X_OK):
        raise storage.ClaimError(
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
                            lease_seconds: float = lease.LEASE_SECONDS,
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
    key = lease.require_probe_key(probe)
    # Before claiming: a probe this harness cannot measure at all is not
    # a probe worth taking off another agent's candidate list.
    probe_flake.resolve_probe(key)
    if runs < 1:
        raise storage.ClaimError(
            f"--runs must be a positive count, got {runs}")
    # Type, finiteness and positivity first, so a `nan` or an `inf`
    # meets the refusal that names what is wrong with it rather than the
    # floor message — and, more to the point, so neither reaches an
    # ordering comparison that would wave it through.
    lease_seconds = lease.require_lease(lease_seconds, "a claim lease")
    result_target = check_result_path(result_path)
    if lease_seconds < MIN_ORCHESTRATION_LEASE_SECONDS:
        raise storage.ClaimError(
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
        claim = lease.acquire(key, root=root, lease_seconds=lease_seconds,
                        repo_root=repo_root)
    except lease.ClaimDenied as denied:
        return Outcome(outcome="already-claimed", probe=key,
                       exit_code=EXIT_ALREADY_CLAIMED, denied=denied,
                       detail=denied.describe())

    with claim:
        target = (Path(census_path) if census_path is not None
                  else probe_census.manifest_path(repo_root))
        record = claim.census_record(commit_sha=storage.commit_sha(repo_root),
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
        except lease.ClaimLost as error:
            raise ClaimLostDuringRun(
                f"probe {key!r}: the claim was lost while its acquisition "
                f"was being recorded ({error}), so another agent now owns "
                f"this probe; it was not run and nothing was recorded in "
                f"{target}",
                phase=PHASE_BEFORE_MEASUREMENT) from None

        with lease.Renewer(claim, renew_interval) as renewer:
            # The last thing before the probe starts: still ours, still
            # live, lease refreshed. Anything at all may have happened
            # between the audit and here — a suspended machine, a
            # census write that blocked for an hour — and starting a
            # measurement this run no longer owns is precisely the
            # duplicated work the claim exists to prevent. A stale
            # answer is worth nothing, so this is a fresh read.
            try:
                claim.reassert()
            except lease.ClaimLost as error:
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
        except lease.ClaimLost as error:
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
