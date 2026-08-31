#!/usr/bin/env python3
"""`/deflake`: one bounded probe census measurement, end to end (#1436).

Five components each answer one question and none of them runs anything:
`probe_select` (#1435) picks a probe, `probe_claim` (#1434) makes it
this agent's, `probe_resource_lock` (#1436) keeps a foreign engine out
of the tracked files it touches, `probe_flake` (#1425) measures it, and
`probe_census` (#1428/#1429) records the result. This is the
orchestration that puts them in order:

    python3 tools/deflake.py

No arguments, at most one probe, exactly one measurement. Repetition
across probes belongs to a surrounding workflow — keeping one
invocation to one probe is what keeps ownership, artifacts and census
updates bounded, and what makes every failure below attributable to a
single measurement.

The fixed measurement contract
------------------------------
Ten sequential runs, four RTS capabilities. The two are INDEPENDENT
dimensions and `-N4` is the second, not the first: the engine's baked-in
default is `-N -A128M`, so an unpinned run takes every core on the
machine and measures a different condition on every machine. Both values
are supplied to `probe_flake.measure` EXPLICITLY rather than left to its
defaults, because "the orchestrator declined to override a default" is
not a property a test can assert about an invocation.

The run count is `probe_census.POLICY_RUN_COUNT` rather than a private
10. It is the same N the selection ladder compares a cohort against, so
a private copy that drifted would leave `/deflake` producing cohorts the
selector permanently classified as incomplete.

Ownership, in order, and what each step may not do
--------------------------------------------------
1. SELECT. `probe_select` is a pure decision over supplied inputs, so
   the census document, the live claim set and the evaluation time are
   read here and passed in. A malformed census — or a `docs-wip`
   worktree that cannot be resolved at all — fails before anything is
   claimed, acquired, measured or written.
2. CLAIM. Acquired before any artifact directory or probe process
   exists. Losing the selection-to-claim race is an ordinary no-work
   success: another agent got there first, so this invocation measures
   nothing, writes nothing, releases nothing of theirs, and does NOT
   pick a second probe.
3. AUDIT. The acquisition is recorded in the census BEFORE the probe
   runs. A measurement nobody can attribute to an acquisition is worse
   than one that did not happen, so a failure here releases the claim
   and refuses to run.
4. PREPARE. The engine executable the runs will launch, built and
   located HERE — before the resource hold below, and outside every
   probe process. A probe that was handed no executable prepares its
   own (#1913), which takes `cabal-build` exclusively; doing that
   underneath this measurement's own hold of the same resource is an
   upgrade that can only deadlock, so the ordering is the fix. It also
   means one build serves all ten runs and no child makes a Cabal
   contact at all. A failure releases the claim and refuses to run.
5. RESOURCES. The probe's declared shared and exclusive interests, taken
   across processes. `run_probes.ResourceLedger` coordinates only the
   probes inside one runner process, so without this a `/deflake`
   measurement and a `tools/run_probes.py` sweep would happily boot two
   engines into the same tracked `config/` tree. Non-blocking here: a
   conflict is reported as `resource-busy` and the claim is given back,
   because an agent that waited would hold a probe hostage for however
   long a foreign sweep takes.

   A SUCCESS-SHAPED OUTCOME OWNS NOTHING. `resource-busy` and
   `claim-busy` are exit 0, which a surrounding workflow reads as
   "nothing happened, move on" — so if giving the claim back fails, the
   retained ownership becomes the result rather than a footnote on it,
   and the outcome is the nonzero `managed-error`. `_no_work` is the one
   funnel every such exit goes through, so that holds by construction.
6. MEASURE. Held resources and a renewed claim throughout, released only
   once the harness has stopped and reaped its process groups.
7. RECORD. The measurement is retained on disk FIRST — it is the
   expensive thing, and everything after it is cheap — then ingested
   under one hold of the claim's sidecar lock.
8. RELEASE. Only what this invocation acquired, and never a successor's.
9. HAND OFF. LAST, and only on the exact `recorded` outcome (#1659).

The handoff
-----------
`deflake-handoff/v1`, written beside the retained result and named after
it, is the record the diagnosis step consumes. It exists because the
alternative is a HAND-WRITTEN account of a run nobody observed: a
consumer given one has to detect a forged record field by field, which
has no natural stopping point. Every value here comes from what this
process saw — the argv and working directory captured at CLI entry, the
configuration read under the resource hold immediately before the first
engine, the base port of each completed run, the two settings passed to
the measurement adapter, and the result document itself, EMBEDDED
unchanged because its schema rejects additional properties.

The acceptable-failure count comes from the census row this invocation's
own recording transaction INSTALLED, which is why that seam now answers
`(probe, installed_census)`. The lock is released the moment the call
returns, so a reread here could pick up another agent's policy edit and
attribute it to this measurement.

Written LAST for a reason. `commit-changed`, `record-failed`,
`record-indeterminate`, `recorded-release-failed` and `harness-error` can
all happen AFTER the measurement, and a diagnosis reads a handoff as a
complete, attributable census measurement — which is exactly what each of
those outcomes says this is not. So the write happens after census
ingestion and after a successful release, and only then does the
invocation report `recorded`.

A `recorded` outcome does not guarantee a retained result to sit beside:
`probe_claim.retain_measurement` can fail while the ingestion that
follows it succeeds. That, and a handoff that cannot be written, are the
nonzero `managed-error` — the census update has already committed, is
append-only, and is neither retried nor rolled back, so both facts are
reported and `handoff_document` stays null. The outcome vocabulary does
not grow: an eleventh step that invented a new one would make every
caller's branch table wrong.

The commit cohort
-----------------
`probe_census.ingest_result` keys a cohort on the RESULT DOCUMENT's own
`commit_sha`, which `probe_flake.Measurement.__init__` captures once
before the first run. That stays the value that is ingested; this
orchestrator does not push a second one at the recorder. What it adds is
a REFERENCE: the commit is captured here too, once, immediately after
the claim is acquired, and immediately before recording all three of
that reference, the result document's own value and a fresh `HEAD` read
must agree. Ten engine-booting runs occupy many minutes to over an hour
and the PR drainer fast-forwards the primary checkout after every merge,
so a measurement really can straddle a HEAD change — and attributing it
to one false cohort is exactly what makes a census number lie. A
disagreement, or a value that does not name a commit at all, is the
nonzero `commit-changed` outcome: nothing recorded, ownership released,
artifacts kept.

Recorder outcomes are decided by a SIGNAL, never by a message
-------------------------------------------------------------
`probe_census.update` guarantees the census bytes are unchanged only
for a failure BEFORE its `os.replace`. The directory fsync that makes
the rename durable sits after it, and used to reach a caller as a bare
`OSError` indistinguishable from a staging-write failure. #1436 added
`probe_census.CensusDurabilityUnconfirmed` for exactly this decision, so
the two sides are told apart deterministically:

* `record-failed` — before the replacement. The census is unchanged on
  #1428's own guarantee, ownership is released, artifacts are retained,
  and the measurement is NOT reported as recorded.
* `record-indeterminate` — after it. The update may already be
  committed, so the claim is deliberately LEFT for token-aware
  diagnostics and TTL recovery, nothing is retried, no compensating
  record is appended, and no claim is made about the census bytes.
  Census ingestion is append-only and deliberately non-idempotent, so an
  automatic retry here would duplicate the sample.

And if the census update commits but releasing the claim then fails, the
answer is `recorded-release-failed`: report BOTH facts, leave the claim
for TTL recovery, and never roll back or repeat a committed append-only
update.

Real runs are evidence, never a gate
------------------------------------
See `tools/README.md`. A completed real ten-run invocation is useful
supplemental evidence on a pull request; it is not required by CI,
branch protection or merge, and no failure rate this lab measures is
itself a merge verdict. The deterministic self-test
`tools/test_deflake.py` is the gate, and it boots no engine.

Machine-readable outcomes
-------------------------
Every invocation ends with exactly one stable identifier. Successful,
exit 0: `recorded`, `no-qualifying-probe`, `claim-busy`, `resource-busy`.
Nonzero: `selector-error`, `claim-audit-failed`, `commit-changed`,
`harness-error`, `record-failed`, `record-indeterminate`,
`recorded-release-failed`, `managed-error`, `interrupted`. Every no-work
result says why no measurement began; every nonzero result states which
ownership, if any, remains.
"""
from __future__ import annotations

import argparse
import datetime
import hashlib
import json
import os
import sys
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_census  # noqa: E402
import probe_claim  # noqa: E402
import probe_engine  # noqa: E402
import probe_flake  # noqa: E402
import probe_protocol  # noqa: E402
import probe_resource_lock  # noqa: E402
import probe_select  # noqa: E402
import run_probes  # noqa: E402

# One census measurement, in runs. Taken from the policy module rather
# than restated: it is the same N the selection ladder measures a cohort
# against, and a private copy that drifted would have `/deflake`
# producing cohorts the selector always calls incomplete.
CENSUS_RUN_COUNT = probe_census.POLICY_RUN_COUNT

# RTS capabilities per run. Supplied EXPLICITLY on every invocation —
# it happens to equal `probe_flake.DEFAULT_RTS_CAPS`, and relying on
# that would make "the orchestrator supplies four capabilities" a
# property of the harness's defaults rather than of this command.
RTS_CAPABILITIES = 4

# The two settings `probe_flake.measure` would otherwise apply from its
# own module constants. Passed EXPLICITLY for the same reason the run
# count and capability count are: "the orchestrator declined to override
# a default" is not a property a test can assert about an invocation, and
# #1659's handoff has to record the values this command actually used.
TIMEOUT = probe_flake.DEFAULT_TIMEOUT
START_PORT = probe_flake.PORT_MIN

# The handoff (#1659): the record the diagnosis step consumes, written
# by the process that performed the measurement rather than assembled
# afterwards from an account of it.
HANDOFF_SCHEMA = "deflake-handoff/v1"

# The gitignored per-worktree configuration family. Probes symlink the
# invoking worktree's `config/` into their isolated resource roots, so
# two comparison worktrees that disagree here measure two different
# conditions — which is why a diagnosis has to know what this
# measurement ran under.
CONFIG_GLOB = "config/*.local.yaml"

OUTCOME_RECORDED = "recorded"
OUTCOME_NO_QUALIFYING_PROBE = "no-qualifying-probe"
OUTCOME_CLAIM_BUSY = "claim-busy"
OUTCOME_RESOURCE_BUSY = "resource-busy"
OUTCOME_SELECTOR_ERROR = "selector-error"
OUTCOME_CLAIM_AUDIT_FAILED = "claim-audit-failed"
OUTCOME_COMMIT_CHANGED = "commit-changed"
OUTCOME_HARNESS_ERROR = "harness-error"
OUTCOME_RECORD_FAILED = "record-failed"
OUTCOME_RECORD_INDETERMINATE = "record-indeterminate"
OUTCOME_RECORDED_RELEASE_FAILED = "recorded-release-failed"
OUTCOME_MANAGED_ERROR = "managed-error"
OUTCOME_INTERRUPTED = "interrupted"

# The four outcomes that mean "this invocation behaved correctly and
# there is nothing more for it to do".
SUCCESS_OUTCOMES = frozenset({
    OUTCOME_RECORDED, OUTCOME_NO_QUALIFYING_PROBE, OUTCOME_CLAIM_BUSY,
    OUTCOME_RESOURCE_BUSY,
})

# This command's own exit statuses. Deliberately NOT `probe_claim.py`'s
# CLI codes: that tool answers "did this claimed measurement run", this
# one answers "what did one census invocation do", and the two disagree
# about `already-claimed` in particular — a lost race is a failure there
# and an ordinary success here. The IDENTIFIER is the contract; the
# number is a convenience for a shell.
EXIT_CODES = {
    OUTCOME_RECORDED: 0,
    OUTCOME_NO_QUALIFYING_PROBE: 0,
    OUTCOME_CLAIM_BUSY: 0,
    OUTCOME_RESOURCE_BUSY: 0,
    OUTCOME_SELECTOR_ERROR: 2,
    OUTCOME_CLAIM_AUDIT_FAILED: 3,
    OUTCOME_COMMIT_CHANGED: 4,
    OUTCOME_HARNESS_ERROR: 5,
    OUTCOME_RECORD_FAILED: 6,
    OUTCOME_RECORD_INDETERMINATE: 7,
    OUTCOME_RECORDED_RELEASE_FAILED: 8,
    OUTCOME_MANAGED_ERROR: 9,
    OUTCOME_INTERRUPTED: 130,
}

# What is still owned when the invocation ends. `claim-held` is not a
# failure to clean up: two outcomes leave it deliberately, for TTL
# recovery and token-aware diagnostics.
OWNERSHIP_NONE = "none"
OWNERSHIP_CLAIM_HELD = "claim-held"


class Result:
    """One invocation's outcome, and everything a caller has to know.

    `outcome` is the stable identifier a surrounding workflow branches
    on; `detail` is for a person. `ownership` says what this process is
    still holding, so an operator never has to guess whether to wait for
    a lease or investigate now.
    """

    def __init__(self, outcome: str, *, probe: str | None = None,
                 detail: str = "", ownership: str = OWNERSHIP_NONE,
                 measurement=None, census_path=None, token: str | None = None,
                 commit: str | None = None, result_path=None,
                 artifacts=(), skipped=(), rung: int | None = None,
                 conflict=None, runs: int | None = None,
                 rts_capabilities: int | None = None, handoff_path=None):
        self.outcome = outcome
        self.probe = probe
        self.detail = detail
        self.ownership = ownership
        self.measurement = measurement
        self.census_path = census_path
        self.token = token
        self.commit = commit
        self.result_path = result_path
        self.artifacts = list(artifacts)
        self.skipped = list(skipped)
        self.rung = rung
        self.conflict = conflict
        self.runs = runs
        self.rts_capabilities = rts_capabilities
        self.handoff_path = handoff_path

    @property
    def exit_code(self) -> int:
        return EXIT_CODES[self.outcome]

    @property
    def successful(self) -> bool:
        return self.outcome in SUCCESS_OUTCOMES

    def to_document(self) -> dict:
        document = {
            "outcome": self.outcome,
            "probe": self.probe,
            "detail": self.detail or None,
            "ownership": self.ownership,
            "exit_code": self.exit_code,
            "census": str(self.census_path) if self.census_path else None,
            "token": self.token,
            "commit": self.commit,
            "result_document": str(self.result_path) if self.result_path else None,
            # Always present, so a consumer branches on the VALUE rather
            # than on whether the key exists: the sibling path on
            # `recorded`, null on every other outcome.
            "handoff_document": (str(self.handoff_path)
                                 if self.handoff_path else None),
            "retained_artifacts": self.artifacts,
            "runs": self.runs,
            "rts_capabilities": self.rts_capabilities,
            "rung": self.rung,
            "skipped": self.skipped or None,
            "conflict": self.conflict,
        }
        if self.measurement is not None:
            document["status"] = self.measurement.status
            document["completed_runs"] = len(self.measurement.runs)
            document["requested_runs"] = self.measurement.requested_runs
            document["failure_count"] = self.measurement.failure_count
            document["timeout_count"] = self.measurement.timeout_count
            document["failure_rate"] = self.measurement.failure_rate
        return document


# --------------------------------------------------------------------------
# The live inputs the pure selector cannot read for itself
# --------------------------------------------------------------------------
def claimed_probe_keys(*, registry, root, now) -> set:
    """Every probe another agent currently holds a live claim on.

    An OPTIMIZATION, and only that: it keeps the selector from choosing
    a probe whose claim is certainly unavailable, so a busy roster
    reports "everything is claimed" instead of losing a race. The
    authority on ownership is `probe_claim.acquire`, which is why losing
    that race anyway is a first-class outcome rather than an error.

    A claim file that cannot be read at all is treated as OCCUPIED —
    the same safe direction `probe_claim.acquire` takes with one.
    """
    base = Path(root)
    claimed = set()
    if not base.is_dir():
        return claimed
    for key in sorted(registry):
        try:
            document = probe_claim.read_claim(key, root=base)
        except probe_claim.ClaimError:
            claimed.add(key)
            continue
        if document is None:
            continue
        expires = probe_claim.parse_stamp(document.get("expires_at"))
        if expires is not None and now < expires:
            claimed.add(key)
    return claimed


def _skip_report(selection) -> list:
    """The selector's own reasons, as data rather than prose."""
    return [{"probe": skip.key, "reasons": list(skip.reasons)}
            for skip in selection.skipped]


def configuration_manifest(root) -> list:
    """The `config/*.local.yaml` state of the checkout, as a sorted list.

    Every regular file in the family, by repository-relative POSIX path
    and the lowercase SHA-256 of the bytes actually read, sorted by path
    so two manifests compare as data rather than as directory order. An
    absent family is `[]` — a POSITIVE statement that nothing is there,
    which is the expected default in this repository since none of the
    four paths is tracked.

    Captured while the resource hold is active and immediately before
    the measurement begins, so it describes the configuration the runs
    actually read rather than whatever the directory held afterwards.
    """
    base = Path(root if root is not None else run_probes.REPO_ROOT)
    entries = []
    for path in sorted(base.glob(CONFIG_GLOB)):
        if not path.is_file():
            continue
        digest = hashlib.sha256()
        with open(path, "rb") as handle:
            for block in iter(lambda: handle.read(65536), b""):
                digest.update(block)
        entries.append({"path": path.relative_to(base).as_posix(),
                        "sha256": digest.hexdigest()})
    return entries


def handoff_targets(document) -> list:
    """Every declared check any completed run reported FAIL or MISSING.

    Once each, in the descriptor's own order: these are the diagnosis
    inputs, and a set would lose the order the probe declared while a
    per-run list would repeat them.
    """
    seen = set()
    for run in document.get("runs") or []:
        for check, outcome in (run.get("checks") or {}).items():
            if outcome != probe_protocol.PASS:
                seen.add(check)
    return [entry["id"] for entry in document.get("checks") or []
            if entry.get("id") in seen]


def build_handoff(*, result, acceptable_failures, argv, cwd, configuration,
                  artifacts) -> dict:
    """The `deflake-handoff/v1` document for one completed measurement.

    Every value comes from what this process observed. `ports` is the
    ordered BASE port of each completed run — the value the result
    document records — rather than any wider span the harness may have
    reserved beneath it, and `timeout` and `start_port` are the ones
    this command passed to the measurement adapter.

    The result is embedded UNCHANGED. Its declared schema rejects
    additional properties, so it must not gain a field here; a consumer
    that wants provenance reads this envelope's own keys instead.
    """
    return {
        "schema": HANDOFF_SCHEMA,
        "probe": result["probe"],
        "acceptable_failures": acceptable_failures,
        "targets": handoff_targets(result),
        "result": result,
        "invocation": {
            "argv": list(argv),
            "cwd": str(cwd),
            "retries": 0,
            "ports": [run["port"] for run in result.get("runs") or []],
            "timeout": TIMEOUT,
            "start_port": START_PORT,
        },
        "configuration": list(configuration),
        "artifacts": list(artifacts),
    }


HANDOFF_SUFFIX = "-handoff.json"


def handoff_path_for(result_path) -> Path:
    """Where the handoff sits beside the retained result it describes.

    Derived from the result's own name rather than a fixed one, so a
    `--result` that puts two measurements in one directory cannot make
    two handoffs collide.

    From the WHOLE filename, not its stem: `Path.stem` drops the
    extension, so `census.json`, `census.txt` and `census` all stem to
    `census` and would share one handoff, the later measurement
    overwriting the earlier one's. Appending to the full name is
    injective — distinct results in a directory have distinct names, and
    distinct names give distinct handoffs.
    """
    retained = Path(result_path)
    return retained.with_name(f"{retained.name}{HANDOFF_SUFFIX}")


def write_handoff(path, document) -> str | None:
    """Write the handoff ATOMICALLY. Returns a problem description, or None.

    Raises nothing: a handoff that cannot be written is an outcome this
    command reports, never a traceback out of a run whose census update
    has already committed.

    Staged and renamed rather than written in place, because only the
    `recorded` outcome may leave a handoff and a failed write must leave
    NONE. Writing to the final path directly truncates it first, so a
    disk that fills mid-write would leave a partial or empty
    `*-handoff.json` beside the result while this command reported
    `managed-error` and a null `handoff_document` — a later consumer
    would find evidence that says it is a complete measurement and is
    not. `os.replace` is atomic within a filesystem, and the staging
    file is a sibling, so it is the same one.

    The document is serialized BEFORE anything is opened, so a value
    json cannot encode fails without touching the target at all.
    """
    target = Path(path)
    staging = target.with_name(f"{target.name}.partial")
    try:
        payload = json.dumps(document, indent=2, sort_keys=True) + "\n"
    except (TypeError, ValueError) as error:
        return f"could not serialize the handoff for {target} ({error})"
    try:
        target.parent.mkdir(parents=True, exist_ok=True)
        staging.write_text(payload, encoding="utf-8")
        os.replace(staging, target)
    except OSError as error:
        try:
            staging.unlink()
        except OSError:
            # The staging file is named for its target and ends
            # `.partial`, so one left behind is inert: nothing reads it,
            # and the next successful write replaces it.
            pass
        return f"could not write {target} ({error})"
    return None


def _require_commit(value, what: str) -> str:
    """A resolved full commit identity, or None.

    `probe_flake._commit_sha` and `probe_claim.commit_sha` both answer
    the literal string `unknown` when git could not be consulted, and
    `ingest_result` would key a cohort on that string. Refusing it here
    is what stops a fabricated cohort forming.
    """
    try:
        return probe_census.require_commit_identity(value, what)
    except probe_census.CensusError:
        return None


# --------------------------------------------------------------------------
# The orchestration
# --------------------------------------------------------------------------
def measure_next_probe(*, repo_root=None, census_path=None, claim_root=None,
                       namespace=None, artifact_root=None, result_path=None,
                       runs: int = CENSUS_RUN_COUNT,
                       rts_caps: int = RTS_CAPABILITIES,
                       lease_seconds: float = probe_claim.LEASE_SECONDS,
                       now=None,
                       stale_after_seconds=probe_census.DEFAULT_STALE_AFTER_SECONDS,
                       inputs=None, load_census=None, claimed=None,
                       acquire_claim=None, acquire_resources=None,
                       prepare_engine=None,
                       measure=None, record_claim=None, record_result=None,
                       head_commit=None, announce=None, argv=None, cwd=None,
                       read_configuration=None, save_handoff=None) -> Result:
    """Select, claim, measure, record and release ONE probe.

    Every collaborator is a keyword seam defaulting to the real thing,
    so the shipped path is what runs unless a caller deliberately
    substitutes. `tools/test_deflake.py` substitutes all of them, which
    is what lets it assert this ordering — including the arguments the
    harness receives — without booting an engine.
    """
    moment = now if now is not None else datetime.datetime.now(datetime.timezone.utc)
    live = inputs if inputs is not None else probe_select.live_inputs()
    read_census = load_census if load_census is not None else probe_census.load
    take_claim = acquire_claim if acquire_claim is not None else probe_claim.acquire
    take_resources = (acquire_resources if acquire_resources is not None
                      else _acquire_probe_resources)
    prepare = (prepare_engine if prepare_engine is not None
               else _prepare_probe_engine)
    run_measure = measure if measure is not None else probe_flake.measure
    log_claim = record_claim if record_claim is not None else probe_census.record_claim
    # The INSTALLED candidate, not just the probe key: #1659's handoff
    # records the acceptable-failure count from the row this transaction
    # wrote, and a later reread happens after the lock is released.
    log_result = (record_result if record_result is not None
                  else probe_census.record_result_installed)
    read_config = (read_configuration if read_configuration is not None
                   else configuration_manifest)
    store_handoff = save_handoff if save_handoff is not None else write_handoff
    observed_argv = list(sys.argv) if argv is None else list(argv)
    observed_cwd = os.getcwd() if cwd is None else str(cwd)
    read_head = (head_commit if head_commit is not None
                 else (lambda: probe_claim.commit_sha(repo_root)))

    def say(message: str) -> None:
        if announce is not None:
            announce(message)

    # ---- 1. Select ------------------------------------------------------
    # The docs worktree is resolved HERE, before anything is claimed. The
    # same unavailability met after acquisition is `claim-audit-failed`;
    # met at selection time there is nothing to release and nothing has
    # happened, so it is the selector's own error.
    target = None
    try:
        target = (Path(census_path) if census_path is not None
                  else probe_census.manifest_path(repo_root))
        census = read_census(target)
    except probe_census.DocsWorktreeMissing as error:
        return Result(OUTCOME_SELECTOR_ERROR,
                      detail=f"the census could not be reached ({error})")
    except (probe_census.CensusError, ValueError) as error:
        return Result(OUTCOME_SELECTOR_ERROR,
                      detail=f"the census could not be read ({error})",
                      census_path=target)

    try:
        claim_base = (Path(claim_root) if claim_root is not None
                      else probe_claim.repository_claim_root(repo_root))
        held = (set(claimed) if claimed is not None
                else claimed_probe_keys(registry=live["registry"],
                                        root=claim_base, now=moment))
    except probe_claim.ClaimError as error:
        return Result(OUTCOME_MANAGED_ERROR,
                      detail=f"the claim namespace is unusable ({error})",
                      census_path=target)

    try:
        selection = probe_select.select_next_probe(
            registry=live["registry"], ci_eligible=live["ci_eligible"],
            manual_only=live["manual_only"], protocol=live["protocol"],
            census=census, claimed=held, now=moment,
            stale_after_seconds=stale_after_seconds)
    except probe_select.SelectionError as error:
        return Result(OUTCOME_SELECTOR_ERROR, detail=str(error),
                      census_path=target)
    if selection.outcome == probe_select.OUTCOME_MALFORMED:
        return Result(OUTCOME_SELECTOR_ERROR,
                      detail=f"the census cannot be ranked ({selection.error})",
                      census_path=target)
    if selection.outcome == probe_select.OUTCOME_NO_CANDIDATE:
        return Result(OUTCOME_NO_QUALIFYING_PROBE,
                      detail="no registered probe qualifies for a measurement",
                      census_path=target, skipped=_skip_report(selection))
    probe = selection.probe
    say(f"selected {probe!r} (ladder rung {selection.rung})")

    # ---- 2. Claim -------------------------------------------------------
    try:
        claim = take_claim(probe, root=claim_base, lease_seconds=lease_seconds,
                           repo_root=repo_root)
    except probe_claim.ClaimDenied as denied:
        # The selection-to-claim race, lost. Nothing was measured, nothing
        # was written, the winner's claim is untouched, and no second
        # probe is selected.
        return Result(OUTCOME_CLAIM_BUSY, probe=probe,
                      detail=denied.describe(), census_path=target,
                      rung=selection.rung, skipped=_skip_report(selection))
    except probe_claim.ClaimError as error:
        return Result(OUTCOME_MANAGED_ERROR, probe=probe, detail=str(error),
                      census_path=target)

    # From here the claim is owned, and every path below either releases
    # it or says in its outcome that it deliberately did not.
    try:
        return _measure_claimed(
            probe=probe, claim=claim, selection=selection, target=target,
            repo_root=repo_root, namespace=namespace,
            artifact_root=artifact_root, result_path=result_path, runs=runs,
            rts_caps=rts_caps, take_resources=take_resources,
            prepare=prepare, run_measure=run_measure, log_claim=log_claim,
            log_result=log_result, read_head=read_head, say=say,
            argv=observed_argv, cwd=observed_cwd,
            read_configuration=read_config, save_handoff=store_handoff)
    except KeyboardInterrupt:
        problem = _release(claim)
        return Result(OUTCOME_INTERRUPTED, probe=probe, census_path=target,
                      token=claim.token, rung=selection.rung, runs=runs,
                      rts_capabilities=rts_caps,
                      ownership=(OWNERSHIP_CLAIM_HELD if problem
                                 else OWNERSHIP_NONE),
                      detail="interrupted while this invocation owned the "
                             "probe" + (f"; the claim could not be released: "
                                        f"{problem}" if problem
                                        else "; the claim was released"))
    except BaseException:
        # An UNEXPECTED failure: a programming error, or a signal that is
        # not an interrupt. Every managed outcome above RETURNS rather
        # than raises — including the two that deliberately keep the
        # claim — so anything arriving here is none of them, and leaving
        # the probe claimed until its lease expires would take it out of
        # every other agent's reach for no reason. `release` is
        # token-checked, so a successor's claim is still untouched.
        _release(claim)
        raise


def _release(claim) -> str | None:
    """Give the claim back; return the problem if that failed.

    Token-checked by `probe_claim.Claim.release`, so a claim that has
    already lapsed and been taken over is LEFT where it is rather than
    deleted out from under its successor.
    """
    try:
        claim.release()
        return None
    except probe_claim.ClaimError as error:
        return str(error)


def _no_work(outcome: str, claim, *, detail: str, **fields) -> Result:
    """A successful no-work outcome — unless the claim would not let go.

    A success-shaped outcome means "nothing happened here, move on", and
    a surrounding workflow reads exit 0 exactly that way. A claim this
    process is still holding is NOT nothing: the probe stays out of every
    other agent's reach until the lease expires, and reporting that as an
    ordinary no-op hides it behind the one status nobody investigates. So
    a release that fails stops being a footnote on the no-work result and
    becomes the result — a nonzero `managed-error` naming the retained
    ownership and the token that recovers it.

    Every exit-0 outcome that has ever owned a claim goes through here,
    so the invariant "a success-shaped outcome owns nothing" holds by
    construction rather than at each site.
    """
    problem = _release(claim)
    if problem is None:
        return Result(outcome, detail=f"{detail}; no ownership remains",
                      ownership=OWNERSHIP_NONE, **fields)
    return Result(OUTCOME_MANAGED_ERROR, detail=(
        f"{detail}, but the owned claim (token {claim.token}) could not be "
        f"released ({problem}) and is left for TTL recovery"),
        ownership=OWNERSHIP_CLAIM_HELD, **fields)


def _probe_resource_namespace(*, namespace, repo_root=None) -> str:
    """The one namespace both the hold and the preparation resolve in.

    Split out of `_acquire_probe_resources` rather than duplicated:
    `probe_engine.prepare_executable` takes `cabal-build` in the SAME
    namespace this measurement's hold takes it in, and two independent
    resolutions that agreed today would be two places for a future edit
    to make them disagree.
    """
    if namespace is not None:
        return namespace
    if repo_root is None:
        return run_probes.resource_namespace()
    return probe_resource_lock.repository_namespace(repo_root)


def _prepare_probe_engine(*, namespace, repo_root=None, announce=None) -> str:
    """Build and locate the engine BEFORE the measurement takes its hold.

    A measurement runs the probe as a child process, and a child that
    was handed no executable prepares its own (#1913) — which takes
    `cabal-build` EXCLUSIVELY. This process is by then holding that same
    resource, SHARED for an ordinary probe and exclusive for the three
    that drive Cabal themselves, and `run_probes.run_one` strips the
    inherited runner variables on the way down, so the child could
    neither see the ancestor's hold nor upgrade past it: it would wait
    out its whole allowance for a holder that is itself blocked waiting
    for the child.

    Preparing HERE removes that by ordering rather than by exception.
    The exclusive hold is taken and released before this measurement's
    own hold is acquired, so nothing is ever upgraded; the resolved path
    is then installed as the runner's executable, which puts every run
    of the measurement on the prebuilt path and leaves it making no
    Cabal contact at all.
    """
    return probe_engine.prepare_executable(
        repo_root, namespace=namespace, announce=announce)


def _acquire_probe_resources(probe: str, *, namespace, repo_root=None):
    """The probe's declared interests, taken across processes.

    The declarations are `run_probes`'s, read through its own accessors
    so there is one conflict model rather than a second copy here — and
    so is the NAMESPACE, through `run_probes.resource_namespace`, which
    is the single resolution point both sides of the conflict use. Two
    independent resolutions that agreed today would be two places for a
    future edit to make them disagree, and a disagreement here is
    silent: both processes would take a lock and neither would see the
    other.
    """
    token = _probe_resource_namespace(namespace=namespace, repo_root=repo_root)
    return probe_resource_lock.acquire(
        exclusive=run_probes.exclusive_resources(probe),
        shared=run_probes.shared_resources(probe),
        namespace=token, purpose=f"deflake {probe}")


def _build_recorded_handoff(*, measurement, retained, recorded_row, probe,
                            argv, cwd, configuration, artifacts,
                            save_handoff):
    """Write the handoff for a recorded measurement. `(path, problem)`.

    The acceptable-failure count comes from the census row the recording
    transaction INSTALLED, which is why `record_result_installed` hands
    it back: the lock is released the moment that call returns, so a
    reread here could answer with another agent's policy edit and
    attribute it to this measurement.

    A `recorded` outcome does not guarantee a retained result path —
    `probe_claim.retain_measurement` may fail while the census ingestion
    that follows it succeeds — and there is nowhere to put a sibling
    document without one.
    """
    if retained is None:
        return None, ("the completed measurement was not retained, so there "
                      "is no result for a handoff to sit beside")
    answered = recorded_row[0] if recorded_row else None
    if not (isinstance(answered, tuple) and len(answered) == 2
            and isinstance(answered[1], dict)):
        # The seam's contract is `(probe, installed_census)`. Anything
        # else is a caller that substituted the wrong recorder, and
        # saying so beats reaching into it and raising from inside a
        # transaction that has already committed.
        return None, (f"the recording seam answered {answered!r}, not the "
                      f"probe and the census document it installed")
    row = probe_census.find_entry(answered[1], probe)
    census = (row or {}).get("census") or {}
    acceptable = census.get("acceptable_failures")
    if not isinstance(acceptable, int) or isinstance(acceptable, bool):
        return None, (f"the census row this transaction installed names no "
                      f"acceptable-failure count for {probe!r} "
                      f"({acceptable!r})")
    document = build_handoff(
        result=measurement.to_document(), acceptable_failures=acceptable,
        argv=argv, cwd=cwd, configuration=configuration, artifacts=artifacts)
    path = handoff_path_for(retained)
    problem = save_handoff(path, document)
    return (None, problem) if problem is not None else (path, None)


def _measure_claimed(*, probe, claim, selection, target, repo_root, namespace,
                     artifact_root, result_path, runs, rts_caps,
                     take_resources, prepare, run_measure, log_claim, log_result,
                     read_head, say, argv, cwd, read_configuration,
                     save_handoff) -> Result:
    """Everything that happens while this invocation owns `probe`."""
    common = {"probe": probe, "census_path": target, "token": claim.token,
              "rung": selection.rung, "runs": runs,
              "rts_capabilities": rts_caps}

    # ---- 3. Capture the cohort reference --------------------------------
    # Once, here: after ownership and before anything long-running, so it
    # names the commit the runs are about to be attributed to. It is a
    # REFERENCE for the drift check below, not a second value pushed at
    # the recorder — `ingest_result` keys the cohort on the result
    # document's own `commit_sha`, and two sources for one identity is
    # how they come to disagree.
    captured = _require_commit(read_head(), "the checkout's HEAD")
    if captured is None:
        problem = _release(claim)
        return Result(OUTCOME_COMMIT_CHANGED, detail=(
            "the checkout's commit could not be resolved, so a measurement "
            "would be recorded into a fabricated cohort; nothing was run"
            + (f" (the claim could not be released: {problem})" if problem else "")),
            ownership=OWNERSHIP_CLAIM_HELD if problem else OWNERSHIP_NONE,
            **common)

    # ---- 4. Audit the acquisition ---------------------------------------
    record = claim.census_record(commit_sha=captured, requested_runs=runs)
    try:
        claim.commit_while_held(lambda: log_claim(target, probe, record))
    except probe_claim.ClaimLost as error:
        # Acquired, then lost before anything ran. Nothing was measured
        # and nothing was written, which is the same shape as losing the
        # race in the first place. `release` is token-checked, so the
        # successor's claim is not touched.
        return _no_work(OUTCOME_CLAIM_BUSY, claim, detail=(
            f"the claim was lost while its acquisition was being recorded "
            f"({error}); the probe was not run"), **common)
    except (probe_census.CensusError, probe_census.DocsWorktreeMissing,
            probe_census.CensusDurabilityUnconfirmed, OSError) as error:
        problem = _release(claim)
        return Result(OUTCOME_CLAIM_AUDIT_FAILED, detail=(
            f"the claim was acquired but could not be recorded in the census "
            f"at {target} ({error}); the probe was not run"
            + (f", and the claim could not be released: {problem}"
               if problem else " and the claim was released")),
            ownership=OWNERSHIP_CLAIM_HELD if problem else OWNERSHIP_NONE,
            commit=captured, **common)

    # ---- 5. The engine the runs will launch (#1913) ----------------------
    # BEFORE the hold below, never inside it. Preparation is a Cabal
    # writer and takes `cabal-build` exclusively; this measurement is
    # about to hold that same resource, so preparing under it would be
    # an upgrade that can only deadlock. Resolved once here, the path
    # goes to every run of the measurement through the runner's own
    # executable handoff, so no child performs a Cabal contact of its
    # own — which is also what stops each of N runs rebuilding.
    try:
        engine_exe = prepare(namespace=_probe_resource_namespace(
            namespace=namespace, repo_root=repo_root),
            repo_root=repo_root, announce=say)
    except (probe_engine.EngineExecutableError,
            probe_resource_lock.ResourceLockError) as error:
        problem = _release(claim)
        return Result(OUTCOME_MANAGED_ERROR, detail=(
            f"the engine the runs would launch could not be prepared "
            f"({error}); the probe was not run"),
            ownership=(OWNERSHIP_CLAIM_HELD if problem else OWNERSHIP_NONE),
            commit=captured, **common)
    # `run_probes.run_one` reads this module global for the executable it
    # hands each child through `$SYNARCHY_PROBE_ENGINE_EXE`, and it is
    # the same fill-in `run_probes.main` performs after ITS preflight —
    # `probe_flake` sits between us and `run_one` and forwards nothing of
    # its own, so this is where a de-flake measurement declares which
    # binary its runs are measuring.
    run_probes.ENGINE_EXECUTABLE = engine_exe
    say(f"engine: {engine_exe}")

    # ---- 6. Resources ---------------------------------------------------
    try:
        hold = take_resources(probe, namespace=namespace, repo_root=repo_root)
    except probe_resource_lock.ResourceBusy as busy:
        # A no-work success: somebody else is legitimately using the same
        # repository state. The claim is given back, nothing is created,
        # no other owner's interest is touched, and no second probe is
        # selected.
        return _no_work(OUTCOME_RESOURCE_BUSY, claim,
                        detail=f"{busy.describe()}; the probe was not run",
                        conflict=busy.to_document(), commit=captured, **common)
    except probe_resource_lock.ResourceLockError as error:
        problem = _release(claim)
        return Result(OUTCOME_MANAGED_ERROR, detail=(
            f"the probe's resource interests could not be coordinated "
            f"({error}); the probe was not run"),
            ownership=OWNERSHIP_CLAIM_HELD if problem else OWNERSHIP_NONE,
            commit=captured, **common)

    measurement = None
    try:
        with hold:
            # ---- 7. Measure ---------------------------------------------
            # The last check before an hour of engine time: still ours,
            # still live, lease refreshed. Starting a measurement this
            # run no longer owns is precisely the duplicated work the
            # claim exists to prevent.
            try:
                claim.reassert()
            except probe_claim.ClaimLost as error:
                return _no_work(OUTCOME_CLAIM_BUSY, claim, detail=(
                    f"the claim was lost before the measurement started "
                    f"({error}); the probe was not run"),
                    commit=captured, **common)
            # Under the hold and before the first engine: this is the
            # configuration the runs will actually read, and reading it
            # afterwards would describe whatever the directory held once
            # they were done.
            #
            # It OPENS files, so it can fail the way reading a file
            # fails — unreadable, or gone between the directory listing
            # and the open. That is a managed pre-measurement failure
            # like any other: the claim goes back, no engine starts, and
            # there is no handoff, because a handoff describes a
            # measurement. Letting the `OSError` escape would leave this
            # command with no outcome at all, having already taken and
            # given back ownership.
            try:
                configuration = read_configuration(repo_root)
            except OSError as error:
                problem = _release(claim)
                return Result(OUTCOME_MANAGED_ERROR, detail=(
                    f"the configuration the runs would read could not be "
                    f"captured ({error}); the probe was not run, so nothing "
                    f"was measured, recorded or handed off"),
                    ownership=(OWNERSHIP_CLAIM_HELD if problem
                               else OWNERSHIP_NONE),
                    commit=captured, **common)
            say(f"measuring {probe!r}: {runs} runs at {rts_caps} RTS "
                f"capabilities")
            with probe_claim.Renewer(claim):
                measurement = run_measure(probe, runs,
                                          artifact_root=artifact_root,
                                          rts_caps=rts_caps,
                                          timeout=TIMEOUT,
                                          start_port=START_PORT)
    except (probe_flake.Rejection, probe_flake.PortExhausted) as error:
        problem = _release(claim)
        return Result(OUTCOME_MANAGED_ERROR,
                      detail=f"the measurement could not be run ({error})",
                      ownership=OWNERSHIP_CLAIM_HELD if problem else OWNERSHIP_NONE,
                      commit=captured, **common)
    except KeyboardInterrupt:
        problem = _release(claim)
        return Result(OUTCOME_INTERRUPTED,
                      detail="interrupted while the probe was being measured",
                      ownership=OWNERSHIP_CLAIM_HELD if problem else OWNERSHIP_NONE,
                      commit=captured, **common)
    # The resources are released by the `with` above, which is reached
    # only once the harness has returned — and it returns only after
    # `run_one` has reaped each run's whole process group.

    # ---- 8. Retain, before anything that can fail -----------------------
    retained, retain_problem = probe_claim.retain_measurement(
        measurement, result_path)
    artifacts = measurement.retained_artifacts()
    common["measurement"] = measurement
    common["result_path"] = retained
    common["artifacts"] = artifacts
    kept = (f"the completed measurement was retained at {retained}, and "
            f"`python3 tools/probe_census.py --record {retained}` ingests it "
            f"once the cause is fixed" if retained is not None else
            f"the completed measurement could NOT be retained ({retain_problem})")

    # ---- 9. The cohort must still be the one that was measured ----------
    reported = measurement.to_document().get("commit_sha")
    # Read ONCE. A second `read_head()` inside the diagnostic below could
    # answer differently again and report a third commit that no step
    # ever compared against.
    head = read_head()
    documented = _require_commit(reported, "the result document's commit")
    current = _require_commit(head, "the checkout's HEAD")
    if documented is None or current is None or {documented, current} != {captured}:
        problem = _release(claim)
        return Result(OUTCOME_COMMIT_CHANGED, detail=(
            f"the measurement cannot be attributed to one commit: it was "
            f"claimed at {captured!r}, the result document names {reported!r} "
            f"and the checkout is now at {head!r}; nothing was recorded and "
            f"{kept}"),
            ownership=OWNERSHIP_CLAIM_HELD if problem else OWNERSHIP_NONE,
            commit=captured, **common)

    # ---- 10. Record -----------------------------------------------------
    # A harness error is recorded too: #1428 appends the non-accepted
    # attempt unconditionally while leaving `current`, `history`, samples
    # and aggregates untouched, so the attempt log stays a complete
    # record of what was tried.
    recorded_row: list = []
    try:
        claim.commit_while_held(
            lambda: recorded_row.append(
                log_result(target, measurement.to_document())))
    except probe_census.CensusDurabilityUnconfirmed as error:
        # AFTER the replacement. The update may already be visible, so
        # nothing is retried, nothing compensating is appended, and the
        # claim is LEFT for token-aware diagnostics and TTL recovery.
        return Result(OUTCOME_RECORD_INDETERMINATE, detail=(
            f"the census update at {target} may already be committed and its "
            f"durability could not be confirmed ({error}); it was NOT retried "
            f"and no compensating record was appended, the claim (token "
            f"{claim.token}) is left for TTL recovery, and {kept}"),
            ownership=OWNERSHIP_CLAIM_HELD, commit=captured, **common)
    except probe_claim.ClaimLost as error:
        # Another agent may have been measuring the same probe, so this
        # result is not the exclusive observation the census records.
        problem = _release(claim)
        return Result(OUTCOME_MANAGED_ERROR, detail=(
            f"the claim was lost while the probe was running ({error}), so "
            f"another agent may have been measuring it at the same time; "
            f"nothing was recorded and {kept}"),
            ownership=OWNERSHIP_CLAIM_HELD if problem else OWNERSHIP_NONE,
            commit=captured, **common)
    except (probe_census.CensusError, probe_census.DocsWorktreeMissing,
            OSError) as error:
        # BEFORE the replacement — every remaining way `update` can fail.
        # `OSError` is caught deliberately rather than left to escape: a
        # staging write or fsync that dies raises one from inside
        # `_atomic_replace`'s try/except, which unlinks the staging file
        # and leaves the authoritative bytes alone, and `update` does not
        # convert it. Catching it here is what makes this classification
        # TOTAL, and it is safe to classify by type precisely because the
        # one post-replacement failure now has a type of its own and is
        # handled above — it is not an `OSError` subclass, so no ordering
        # accident can route it here.
        #
        # #1428 guarantees the authoritative bytes are unchanged; the
        # measurement is retained rather than repeated, because
        # re-running the probe is never the recovery.
        problem = _release(claim)
        return Result(OUTCOME_RECORD_FAILED, detail=(
            f"the measurement completed but could not be recorded in the "
            f"census at {target} ({error}); the authoritative census bytes "
            f"are unchanged and {kept}"),
            ownership=OWNERSHIP_CLAIM_HELD if problem else OWNERSHIP_NONE,
            commit=captured, **common)

    # ---- 11. Release ---------------------------------------------------
    problem = _release(claim)
    if problem is not None:
        # Committed, then could not let go. Both facts are reported; the
        # append-only update is never rolled back or repeated.
        return Result(OUTCOME_RECORDED_RELEASE_FAILED, detail=(
            f"the measurement WAS recorded in the census at {target}, and "
            f"releasing the claim (token {claim.token}) then failed "
            f"({problem}); the claim is left for TTL recovery and the "
            f"committed census update was neither rolled back nor repeated"),
            ownership=OWNERSHIP_CLAIM_HELD, commit=captured, **common)

    if not measurement.valid:
        return Result(OUTCOME_HARNESS_ERROR, detail=(
            f"the measurement's protocol stream could not be trusted "
            f"({measurement.error}); the non-accepted attempt was recorded "
            f"and no cohort, sample or aggregate changed — {kept}"),
            commit=captured, **common)

    # ---- 12. Hand off --------------------------------------------------
    # LAST, and only on the exact `recorded` outcome. Every step above
    # that returns early — `commit-changed`, `record-failed`,
    # `record-indeterminate`, `recorded-release-failed`, `harness-error`
    # — can happen AFTER the measurement, and none of them may leave a
    # handoff behind: the diagnosis step reads one as a complete,
    # attributable census measurement, which is exactly what those
    # outcomes say this is not.
    document, problem = _build_recorded_handoff(
        measurement=measurement, retained=retained, recorded_row=recorded_row,
        probe=probe, argv=argv, cwd=cwd, configuration=configuration,
        artifacts=artifacts, save_handoff=save_handoff)
    if problem is not None:
        # The census update has already committed and is append-only, so
        # nothing here retries it or rolls it back. The claim was
        # released above, so this invocation owns nothing.
        return Result(OUTCOME_MANAGED_ERROR, detail=(
            f"the measurement WAS recorded in the census at {target}, and "
            f"the handoff could not be written ({problem}); the committed "
            f"census update was neither rolled back nor repeated, and "
            f"{kept}"),
            commit=captured, **common)
    return Result(OUTCOME_RECORDED, detail=(
        f"{len(measurement.runs)}/{measurement.requested_runs} runs at "
        f"{rts_caps} RTS capabilities, {measurement.failure_count} failure(s) "
        f"including {measurement.timeout_count} timeout(s)"),
        commit=captured, handoff_path=document, **common)


# --------------------------------------------------------------------------
# CLI
# --------------------------------------------------------------------------
def render(result: Result) -> str:
    lines = [f"{result.outcome}: {result.detail or '(no detail)'}"]
    if result.probe:
        lines.append(f"  probe:     {result.probe}")
    if result.commit:
        lines.append(f"  commit:    {result.commit}")
    if result.token:
        lines.append(f"  token:     {result.token}")
    if result.census_path:
        lines.append(f"  census:    {result.census_path}")
    if result.result_path:
        lines.append(f"  result:    {result.result_path}")
    if result.handoff_path:
        lines.append(f"  handoff:   {result.handoff_path}")
    for artifact in result.artifacts:
        lines.append(f"  artifacts: {artifact}")
    lines.append(f"  ownership: {result.ownership}")
    if result.outcome == OUTCOME_NO_QUALIFYING_PROBE:
        for skip in result.skipped:
            lines.append(f"  skipped {skip['probe']}: "
                         f"{', '.join(skip['reasons'])}")
    return "\n".join(lines)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    # Deliberately no probe selector, no run count and no RTS override:
    # the whole point of the command is that one invocation is one
    # bounded measurement of whichever probe the ladder chose. A forced
    # probe would let an operator manufacture a `recorded` outcome that
    # proves nothing about the selection it skipped.
    parser.add_argument("--json", action="store_true",
                        help="emit the machine-readable outcome document")
    parser.add_argument("--result", default=None, metavar="PATH",
                        help="also write the completed measurement's result "
                             "document here (it is always retained beside "
                             "its artifacts, outside every worktree)")
    args = parser.parse_args(argv)

    def announce(message: str) -> None:
        if not args.json:
            print(message, flush=True)

    try:
        # argv and the working directory are captured HERE, at CLI entry,
        # so the handoff records what this process was actually invoked
        # as rather than what a later frame could reconstruct.
        result = measure_next_probe(result_path=args.result,
                                    announce=announce,
                                    argv=list(sys.argv), cwd=os.getcwd())
    except KeyboardInterrupt:
        # Only reachable before the claim is owned: once it is,
        # `measure_next_probe` handles the interrupt itself so it can
        # report what ownership it gave back.
        result = Result(OUTCOME_INTERRUPTED,
                        detail="interrupted before the measurement began")
    if args.json:
        print(json.dumps(result.to_document(), indent=2, sort_keys=True))
    else:
        print(render(result))
    return result.exit_code


if __name__ == "__main__":
    raise SystemExit(main())
