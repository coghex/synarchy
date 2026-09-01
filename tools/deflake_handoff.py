#!/usr/bin/env python3
"""The `deflake-outcome-handoff/v1` contract both of epic #1426's consumers read.

`tools/deflake_diagnosis.py` (#1437) decides mechanically whether one
measurement handoff supports a probe-side repair, and emits a
`deflake-diagnosis-outcome/v1` record naming the ROUTE it took. Only one
of those routes opens a pull request; the rest are split across two
sibling consumers of ONE envelope:

* `tools/deflake_outcome.py` (#1439) records the three stable
  non-success outcomes — `cannot-reproduce`, `no-confident-fix` and
  `partial-improvement` — durably in the probe's census row.
* `tools/deflake_issue.py` (#1438) files the tracker issue the
  `production-defect` ending calls for.

This module owns what the two SHARE, and nothing either one decides on
its own. It has no CLI, records nothing, publishes nothing, and imports
neither consumer: everything below reads documents and answers questions
about them, so the two workflows cannot come to disagree about what the
envelope means, and neither is the other's prerequisite. A consumer
supplies its own `RouteOwnership` — the one part of the gate that
differs — and keeps its own outcome vocabulary, its own boundaries and
its own CLI.

    python3 tools/test_deflake_diagnosis.py       # the deterministic gate

Its ownership runs from the envelope to the point a validated `Handoff`
is handed back: the schema and the measurement-role vocabulary, route
ownership and what each of #1437's endings IS, the two exception
classifications, the `Measurement` and `Handoff` representations, every
producer-record, identity, manifest, invocation, path, worktree,
descriptor, artifact and producer-binding rule, the shared
reproduced-failure predicate, and the clock and retry reconciler a
durable record is stamped with.

A document that exists establishes nothing
------------------------------------------
`tools/probe_flake.py`'s exit contract is preserved rather than
paraphrased, and the handoff must state each measurement's exit code so
this can be checked instead of inferred:

* exit 0 — a VALID measurement, whatever failure rate it observed. It
  writes a document whose own `status` is `ok`.
* exit 2 — a rejection before execution. It writes NO document.
* exit 3 — port exhaustion. It writes NO document.
* exit 4 — an untrustworthy harness result. It DOES write a document:
  `probe_flake` renders and writes one whenever it reached the
  measurement loop, and returns `EXIT_HARNESS_ERROR` only from
  `return EXIT_OK if measurement.valid else EXIT_HARNESS_ERROR`. That
  document carries `status: "harness-error"`, a populated `error` and
  `error_run`, and a null `failure_rate`.

So a document that merely EXISTS and PARSES establishes nothing.
`Measurement.trustworthiness_problems` reads the document's own
`status`, `error_run`, `completed_runs` and per-check tallies, and any
of exit 2, 3 or 4, an incomplete run set, malformed measurement data or
an inconsistent aggregate is a batch no consumer may conclude anything
from. That is the load-bearing distinction of the whole envelope: "we
could not make it fail" and "we could not measure it" are opposite
conclusions that a careless reading collapses into one.

Every declared measurement goes through #1437's CANONICAL result gate
first, not just the census validator it starts with:
`probe_census.validate_result` owns declared shape and the cross-field
invariants, while `deflake_diagnosis.require_result` adds the rules that
make a document one `probe_flake.measure` could have written — the run
indices, the artifact topology, and the retention pairing in particular.
`measure` deletes a run's directory the moment it passes and keeps every
unsuccessful one, so a non-PASS run with a null `artifact_dir` is
producer-impossible, and a conclusion drawn from one would be a failure
nobody can diagnose stored as the evidence FOR a diagnosis.

`error_run` gets its own clause because `probe_flake` deliberately keeps
a harness-error run OUT of `runs`. A reader inspecting only `runs` would
see an all-PASS list for a measurement nobody can trust.

A document that contradicts ITSELF is refused the same way, and before
any route is classified rather than only before one of them.
`probe_census.validate_result` binds `check_counts` to `runs` and
refuses a PASS run carrying a FAIL check, but nothing there binds
`failure_count`, `timeout_count` or `failure_rate` to the run list — so
an all-PASS batch under a forged failure count is schema-valid, and it
would read as a REPRODUCED failure, which is the evidence every route
past the `cannot-reproduce` fork rests on. The three totals are
therefore reconciled against the run list using
`probe_flake.Measurement`'s own arithmetic, and so is `completed_runs`,
which the producer writes as `len(runs)` and which makes the rest mean
anything: a nine-run batch claiming ten completed satisfies
`completed_runs == requested_runs` and would be STORED as ten of ten. A
mismatch in any of the four is an untrustworthy measurement.

The record has to agree with itself first
-----------------------------------------
#1437's record states the input identity TWICE: its `handoff` section
carries the probe, the commit, X and the targets of the `/deflake`
invocation consumed, and the top-level fields are derived from that same
handoff — `baseline_sha` IS `handoff.commit_sha`. Validating each side
alone leaves the pair free to disagree, and a record whose handoff
identifies one commit while its top-level field, its baseline reference
and the supplied measurement all name another would satisfy every other
check here. Each duplicated field is re-parsed with the grammar its twin
was parsed with and required to match, so agreement is established
between two VALIDATED values rather than two strings.

One attempt reports against one declared contract
-------------------------------------------------
A batch's DESCRIPTOR is what it reports against, and the identity
binding below cannot see it: a result can keep its probe, its targets,
its commit, its instant and every artifact path while swapping or
relabelling an unrelated declared check, which #1437 rejects rather than
routing anywhere. The record carries the WHOLE ordered descriptor — #1437
serializes it beside the identifier list for this — and every supplied
measurement is held to it through that module's own
`require_descriptor`, so identifiers, order and LABELS are compared by
one definition. Against the RECORD rather than against a sibling
measurement, because the routes that carry one baseline have no sibling,
and those are the routes whose consumers record a de-list
recommendation: a self-consistent handoff that relabelled a check would
otherwise recommend de-listing on the strength of a different asserted
check. A label is the check's stated meaning. The targets are held to
that same descriptor: a target it never declared cannot be one of the
measurement's own non-PASS identifiers.

The measurement is the one that diagnosis judged
------------------------------------------------
Binding a declared measurement to its PROBE alone would admit any
well-formed batch of that probe: one taken at another commit, or another
instant, supplied under a diagnosis that judged a different one, leaving
the census holding two conflicting accounts of a single attempt. So each
declared measurement is held to the producer record's own reference for
its role — commit and instant — and the pre-fix roles are held again to
the `baseline_sha` the census row is about to record. Two independent
statements, because a producer record whose reference and `baseline_sha`
disagreed would satisfy either one alone. A role the producer ran no
batch for carries a `null` reference, and a measurement supplied for it
describes work the invocation did not do.

The run count is the document's, not a literal
----------------------------------------------
Completeness is `completed_runs == requested_runs` and the ceiling is X
out of the measurement's OWN requested run count. Ten is the standard
configured N (`probe_census.POLICY_RUN_COUNT`) and nothing here changes
a measurement semantic, but hard-coding it would silently misclassify a
measurement taken at any other run count — which the handoff carries as
an input.

Paths are references, and they stay outside every worktree
----------------------------------------------------------
Raw stdout, protocol streams and engine logs stay in the harness's
artifact tree outside every worktree; only their path references are
stored, exactly as `probe_census.summarize_sample` already does for a
measurement — and every path a consumer would store is REFUSED if it
lies inside one, which `probe_flake.check_artifact_root` enforces at
measurement time and this enforces again at the point a path can enter a
durable record. The comparison worktrees the producer record declares
are checked beside the live registered ones, because `/deflake` removes
them when it finishes and an artifact that sat inside one was still
inside a worktree when it was written.

Absolute is not the same as usable. Every path goes through
`deflake_diagnosis.require_path` first — including the ones a consumer
stores without ever resolving, a command token and a manifest entry
among them, because those are evidence too. An embedded NUL makes
`Path.resolve()` raise `ValueError` from `lstat` rather than `OSError`,
so such a string passes an absoluteness test, names no location for the
containment check to find, and would be stored as an artifact reference
or an invocation directory while the CLI printed a traceback.

The producer record's own artifact list is REBUILT rather than believed.
#1437 produces it by accumulating every batch it ran, in role order,
deduplicated — so it is derived, and validating each path individually
only asks whether each names a legal place, never whether the list is
the one the invocation produced. An unrelated directory appended to it
alone would otherwise be stored as this attempt's evidence.

The two inputs no census field has ever held are validated here too: the
configuration manifest both batches read, and the exact command and
directory of the `/deflake` invocation the diagnosis consumed.
`probe_census.ingest_result` drops the command and the invocation
directory, and nothing in the census stores the manifest at all, so a
record without them could not say what was run or under what state —
which is the first thing anyone resuming an attempt needs.

The clock is shared because the resume rule is
----------------------------------------------
Both consumers write a durable record keyed on the attempt identity, and
both are idempotent on it. Exactly one field of such a record is not
derived from the handoff — `timestamp_utc` comes from a clock, which
reads differently on a retry — so `utc_now` and `reuse_stored_timestamp`
live here rather than once per consumer. A second reconciler is how one
of them would come to restamp a replay into a conflict.
"""
from __future__ import annotations

import copy
import os
import sys
from datetime import datetime, timezone
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import deflake_diagnosis  # noqa: E402
import probe_census  # noqa: E402
import probe_flake  # noqa: E402
import probe_protocol  # noqa: E402
import probe_runner_registry  # noqa: E402

HANDOFF_SCHEMA = "deflake-outcome-handoff/v1"
# The producer record this consumes, named from the producer rather than
# restated: a second copy that drifted would accept a document #1437
# never writes.
DIAGNOSIS_OUTCOME_SCHEMA = deflake_diagnosis.OUTCOME_SCHEMA

# The measurement roles a handoff may declare.
ROLE_HANDOFF = "handoff"
ROLE_BASELINE = "baseline"
ROLE_VERIFICATION = "verification"
ROLES = (ROLE_HANDOFF, ROLE_BASELINE, ROLE_VERIFICATION)
# The roles measured BEFORE any repair, and therefore at the diagnosis
# outcome's own baseline commit. The verification is deliberately not
# one: #1437 requires it to be measured at the REPAIR commit.
PRE_FIX_ROLES = (ROLE_HANDOFF, ROLE_BASELINE)

# What each of #1437's endings IS, in a sentence. A consumer that does
# not own a route still has to say what it was handed, and a bare
# identifier is not that; the workflow that DOES own it comes from
# `deflake_diagnosis.ROUTE_OWNER` rather than from a second table here.
ROUTE_ENDING = {
    deflake_diagnosis.ROUTE_NO_TARGET:
        "observed no non-PASS check at all",
    deflake_diagnosis.ROUTE_CANNOT_REPRODUCE:
        "could not reproduce the failure under the handoff's own condition",
    deflake_diagnosis.ROUTE_PRODUCTION_DEFECT:
        "identifies a production-code or shipped-script defect",
    deflake_diagnosis.ROUTE_NO_CONFIDENT_FIX:
        "reproduced the failure and established no one bounded probe-side "
        "repair",
    deflake_diagnosis.ROUTE_PARTIAL_IMPROVEMENT:
        "measurably improved the failure count without passing the "
        "acceptance gate",
    deflake_diagnosis.ROUTE_REPAIR:
        "is #1437's confidently diagnosed repair",
    deflake_diagnosis.ROUTE_HANDOFF_REJECTED:
        "never reached a diagnosis at all",
}


class RouteOwnership:
    """Which of #1437's routes ONE consuming workflow owns.

    Epic #1426 splits the non-repair endings across two sibling
    consumers of the same `deflake-outcome-handoff/v1` envelope: this
    module records the three STABLE outcomes (#1439), and
    `tools/deflake_issue.py` files a tracker issue for the
    production-defect ending (#1438). Everything the two share — the
    producer record, the measurement binding, the descriptor, the
    worktree boundary, the artifact rebuild — is checked by ONE entry
    gate, and the owned-route table is the only part of it that differs.

    So the gate takes the table as a parameter instead of existing
    twice. A second copy would be the one that went stale, and a rule
    duplicated into a sibling is a rule that sibling can weaken
    privately. Nothing here imports, requires or stubs #1438: the
    ownership object is data each consumer supplies, and one
    consumer's instance is unchanged by anyone constructing another.
    """

    def __init__(self, *, issue: int, outcomes, roles: dict):
        self.issue = issue
        self.outcomes = tuple(outcomes)
        self.roles = dict(roles)

    def owns(self, route) -> bool:
        return route in self.roles

# `tools/probe_flake.py`'s exit contract, taken from the module that
# owns it. Each exit says whether a result document exists at all and,
# when one does, what its own status must be — so "the document exists"
# is never mistaken for "the measurement is trustworthy".
EXIT_CONTRACT = {
    probe_flake.EXIT_OK: probe_census.ACCEPTED_STATUS,
    probe_flake.EXIT_REJECTED: None,
    probe_flake.EXIT_NO_PORT: None,
    probe_flake.EXIT_HARNESS_ERROR: "harness-error",
}

MAX_IDENTITY = 128
MAX_SUMMARY = 8000

class HandoffError(Exception):
    """An outcome handoff this workflow refuses to read.

    Distinct from a non-success on purpose: a malformed input never
    reached a classification, so it is not a `cannot-reproduce` or a
    `no-confident-fix` and must not be recorded as one. It publishes no
    pull request and records no completed stable outcome.
    """


class NonSuccess(Exception):
    """A well-formed handoff whose ending is not a stable outcome.

    Actionable: the message names what the evidence actually showed and
    which route owns it. Nothing is recorded, nothing is published, and
    the attempt stays resumable.
    """


# ==========================================================================
# The entry gate
# ==========================================================================

def _require_object(value, what: str) -> dict:
    if not isinstance(value, dict):
        raise HandoffError(
            f"{what} must be a JSON object, got {type(value).__name__}")
    return value


def _require_text(value, what: str, *, limit: int) -> str:
    if not isinstance(value, str) or not value.strip():
        raise HandoffError(f"{what} must be a non-empty string, got {value!r}")
    if len(value) > limit:
        raise HandoffError(
            f"{what} is {len(value)} characters, above the {limit} a census "
            f"record stores")
    return value


def _require_identity(value, what: str) -> str:
    """An attempt identity: one line, no surrounding blanks, bounded.

    It is the IDEMPOTENCY key, so it has to be something a resuming
    workflow can reproduce exactly. A value carrying a newline or
    padding is one that two spellings of the same attempt would differ
    in, which is how a resume silently becomes a second append.
    """
    text = _require_text(value, what, limit=MAX_IDENTITY)
    if text != text.strip() or any(ch.isspace() and ch != " " for ch in text):
        raise HandoffError(
            f"{what} must be a single unpadded line, got {value!r}")
    return text


def _require_string_list(value, what: str) -> list:
    if not isinstance(value, list) or not all(
            isinstance(item, str) and item for item in value):
        raise HandoffError(f"{what} must be a list of non-empty strings")
    return list(value)


def delegate_census_grammar(call, what: str):
    """A census grammar applied here, reported as this gate's refusal.

    Commit identities and timestamps are the census's own vocabulary and
    are matched full-string by it, the placeholder `unknown` included. A
    `CensusError` is that grammar refusing the input, so it is reported
    as a malformed handoff rather than escaping as a traceback from a
    module the caller never named.
    """
    try:
        return call()
    except probe_census.CensusError as error:
        raise HandoffError(f"{what}: {error}") from None


def _require_nul_free(value, what: str) -> str:
    """A recorded string the filesystem could actually name.

    Split out from `_require_usable_path` because the same rule applies
    to strings this module STORES without ever resolving — a command
    token, a manifest entry's path — and to the ones it resolves. Both
    are evidence, and evidence naming nothing is not evidence.
    """
    return _require_usable_path(value, what)


def _require_usable_path(value, what: str) -> str:
    """A path string the FILESYSTEM can be asked about, via #1437's rule.

    Being a non-empty absolute-looking string is not enough. An embedded
    NUL makes `Path.resolve()` raise `ValueError` from `lstat` rather
    than `OSError`, and `deflake_diagnosis.inside_any_worktree` resolves
    every form it compares — so a NUL-bearing string would sail past the
    absoluteness test, name no filesystem location for the containment
    check to find, and be stored in the census as an artifact reference
    or an invocation directory.

    `deflake_diagnosis.require_path` is that module's own rule for
    exactly this, applied at every point a document-supplied path is
    first read. Calling it keeps one definition of "a path a recorded
    measurement could have used"; its refusal is re-raised as this
    module's own, so a caller never sees the producer's exception type
    escaping from a consumer it did not invoke.
    """
    try:
        return deflake_diagnosis.require_path(value, what)
    except deflake_diagnosis.HandoffError as error:
        raise HandoffError(str(error)) from None


def require_artifact_reference(value, what: str, *, worktrees=()) -> str:
    """One retained-artifact path, stored as a REFERENCE and nothing else.

    Absolute because `probe_flake.check_artifact_root` resolves its root
    before a run begins and every path below it is built from that, so a
    relative entry names nothing a later reader can find. The census
    stores this string; it never opens the directory and never copies a
    byte of what is inside it.

    And OUTSIDE every worktree, which `probe_flake.check_artifact_root`
    refuses at measurement time and #1437 re-checks on every path a
    result names. Restating it here is not redundant: this module is
    what puts a path into a durable record, "raw artifacts do not enter
    a worktree" is one of the guarantees that record carries, and a
    self-consistent handoff that named a worktree-resident tree would
    otherwise pass every other check and be stored. Containment is
    compared over `_path_forms`, so a `..` segment or a symlinked
    spelling of the same place is the same place.
    """
    text = _require_usable_path(
        _require_text(value, what, limit=4096), what)
    if not Path(text).is_absolute():
        raise HandoffError(
            f"{what} must be an absolute path, got {text!r}; the census "
            f"stores artifact references, and a relative one names nothing")
    tree = deflake_diagnosis.inside_any_worktree(text, worktrees)
    if tree is not None:
        raise HandoffError(
            f"{what} is {text!r}, inside the worktree {tree}; raw probe "
            f"artifacts stay outside every worktree, and a census record "
            f"that referenced one would be pointing at evidence a checkout "
            f"can overwrite")
    return text


def require_configuration(value, what: str) -> list:
    """The manifest of gitignored configuration the batches read.

    #1437 validates this and then states it in its record; this requires
    it and STORES it, because "resume without repeating completed work"
    means a later reader can establish which configuration the numbers
    were measured under — and no census field has ever held it.

    Held to #1437's own `require_manifest`, called rather than
    reimplemented: the family rule, the digest shape and the
    duplicate-path rule are that module's, and a second copy is how the
    two would come to disagree about what a manifest entry may be. An
    EMPTY entry list is the expected default and a real statement —
    every `config/*.local.yaml` absent — so the KEY is required and the
    entries are not.

    What is stored is the ENTRIES. The manifest also names the root it
    was scanned from, which is a machine-local scratch path that means
    nothing to a later reader of the census.
    """
    try:
        manifest = deflake_diagnosis.require_manifest(value, what)
    except deflake_diagnosis.HandoffError as error:
        raise HandoffError(str(error)) from None
    for position, entry in enumerate(manifest["entries"]):
        # #1437's family rule is a shape rule — `fnmatch` and
        # `PurePosixPath` neither stat nor reject a NUL — so a
        # `config/x\x00.local.yaml` satisfies it and would be stored as
        # the name of a file that was read.
        _require_nul_free(entry["path"], f"{what} entry {position}'s `path`")
    return [{"path": entry["path"], "sha256": entry["sha256"]}
            for entry in manifest["entries"]]


def require_input_identity(section, what: str, *, probe: str,
                           baseline_sha: str, acceptable_failures: int,
                           targets) -> None:
    """#1437's record states the input identity TWICE; the two must agree.

    Its `handoff` section carries the probe, the commit, X and the
    targets of the `/deflake` invocation that was consumed, and the
    record's own top-level fields are derived from that same handoff —
    `baseline_sha` IS `handoff.commit_sha`. Validating each side alone
    leaves the pair free to disagree: a record whose handoff identifies
    commit A while its top-level `baseline_sha`, its baseline reference
    and the supplied measurement all say B satisfies every check made so
    far, and the census would then hold B as the diagnosed baseline of
    an attempt the producer says was about A.

    Each field is re-parsed with the grammar its top-level twin was
    parsed with, so agreement is established between two VALIDATED
    values rather than between two strings.
    """
    record = _require_object(section, what)
    if record.get("probe") != probe:
        raise HandoffError(
            f"{what}.probe is {record.get('probe')!r} while the diagnosis "
            f"outcome names the probe {probe!r}; one record cannot be about "
            f"two probes")
    commit = delegate_census_grammar(
        lambda: probe_census.require_commit_identity(
            record.get("commit_sha"), f"{what}.commit_sha"),
        f"{what}'s commit")
    if commit != baseline_sha:
        raise HandoffError(
            f"{what}.commit_sha is {commit!r} while the diagnosis outcome's "
            f"`baseline_sha` is {baseline_sha!r}; the baseline commit IS the "
            f"commit the consumed handoff was measured at, so a record whose "
            f"two statements of it disagree identifies no baseline at all")
    ceiling = delegate_census_grammar(
        lambda: probe_census.require_acceptable_failures(
            record.get("acceptable_failures"),
            f"{what}.acceptable_failures"),
        "the consumed handoff's acceptable-failure ceiling")
    if ceiling != acceptable_failures:
        raise HandoffError(
            f"{what}.acceptable_failures is {ceiling!r} while the diagnosis "
            f"outcome's is {acceptable_failures!r}; every classification "
            f"here is made against that ceiling, so the two cannot differ")
    declared = _require_string_list(record.get("targets"),
                                    f"{what}.targets")
    if declared != list(targets):
        raise HandoffError(
            f"{what}.targets is {declared} while the diagnosis outcome's is "
            f"{list(targets)}; the targets are the checks under diagnosis "
            f"and a record that names two sets names neither")


def require_invocation_identity(section, what: str) -> dict:
    """WHICH `/deflake` invocation the diagnosis consumed.

    The exact command and the directory it ran in. The census row cannot
    answer this — `probe_census.ingest_result` drops the command and the
    invocation directory — so a record that did not keep it could not
    say what was actually run, which is the first thing anyone resuming
    an attempt needs.
    """
    record = _require_object(section, what)
    command = _require_string_list(record.get("command"), f"{what}.command")
    if not command:
        raise HandoffError(
            f"{what}.command is empty; a measurement nobody can say the "
            f"command for cannot be repeated")
    for position, token in enumerate(command):
        # The interpreter and the script are paths, and the whole list
        # is stored verbatim as the evidence of what was run. A token no
        # shell could have passed is not a record of a real invocation.
        _require_nul_free(token, f"{what}.command[{position}]")
    directory = _require_usable_path(
        _require_text(record.get("directory"), f"{what}.directory",
                      limit=4096),
        f"{what}.directory")
    if not Path(directory).is_absolute():
        raise HandoffError(
            f"{what}.directory is {directory!r}, not an absolute path")
    return {"command": command, "directory": directory}


class Measurement:
    """One measurement the handoff hands on, with its harness exit.

    The exit code and the document are held TOGETHER because neither
    answers the question alone: exits 2 and 3 write no document, and
    exit 4 writes one that says so itself.
    """

    def __init__(self, role: str, exit_code: int, result):
        self.role = role
        self.exit_code = exit_code
        self.result = result

    # -- the document's own account of itself ------------------------------
    @property
    def requested_runs(self) -> int:
        return self.result["requested_runs"]

    @property
    def completed_runs(self) -> int:
        return self.result["completed_runs"]

    @property
    def failure_count(self) -> int:
        return self.result["failure_count"]

    @property
    def timeout_count(self) -> int:
        return self.result["timeout_count"]

    @property
    def check_counts(self) -> dict:
        return self.result["check_counts"]

    def trustworthiness_problems(self) -> list[str]:
        """Every reason this measurement cannot support a conclusion.

        The complete list rather than the first, because an operator
        reading a non-success needs to know what to fix, and a batch
        that both aborted early and lost a run has two problems.
        """
        if EXIT_CONTRACT[self.exit_code] != probe_census.ACCEPTED_STATUS:
            return [f"the {self.role} measurement exited {self.exit_code}, "
                    f"which is not a valid measurement"]
        problems = []
        if self.result.get("error_run") is not None:
            problems.append(
                f"the {self.role} measurement kept an error run, so one of "
                f"its runs produced a stream nobody can trust")
        if self.result.get("error") is not None:
            problems.append(
                f"the {self.role} measurement reports the harness error "
                f"{self.result['error']!r}")
        if self.completed_runs != self.requested_runs:
            problems.append(
                f"the {self.role} measurement completed "
                f"{self.completed_runs} of {self.requested_runs} requested "
                f"runs")
        problems += self.aggregate_problems()
        return problems

    def aggregate_problems(self) -> list[str]:
        """Every way this document's totals contradict its own run list.

        A document that disagrees with itself is one nobody can believe,
        so this belongs to trustworthiness rather than to any one
        route's predicate: an inconsistent aggregate must be refused
        before ANY stable outcome is reached, not only before
        `cannot-reproduce`.

        Nothing upstream establishes it. `probe_census.validate_result`'s
        cross-field rules bind `check_counts` to `runs` and refuse a
        PASS run carrying a FAIL check, but they say nothing about
        `failure_count`, `timeout_count` or `failure_rate` — so an
        all-PASS run list under a forged failure count is schema-valid,
        passes the census's own invariants, and would otherwise read as
        a reproduced failure.

        `completed_runs` is checked here for the same reason and is the
        one that makes the rest mean anything: the producer writes it as
        `len(runs)`, nothing on the way back in re-establishes that, and
        a nine-run batch claiming ten completed passes a completeness
        test of `completed_runs == requested_runs` and is then STORED as
        ten of ten.

        The arithmetic is `probe_flake.Measurement`'s own, not a second
        opinion: a failure is a FAIL or a TIMEOUT run, a timeout is a
        TIMEOUT run, and the rate is failures over REQUESTED runs
        rounded the way the producer rounds it.
        """
        problems: list[str] = []
        runs = self.result["runs"]
        if len(runs) != self.completed_runs:
            problems.append(
                f"the {self.role} measurement reports "
                f"{self.completed_runs} completed run(s) while its run list "
                f"holds {len(runs)}")
        failures = sum(1 for run in runs if run["outcome"] in
                       (probe_flake.RUN_FAIL, probe_flake.RUN_TIMEOUT))
        timeouts = sum(1 for run in runs
                       if run["outcome"] == probe_flake.RUN_TIMEOUT)
        if self.failure_count != failures:
            problems.append(
                f"the {self.role} measurement reports "
                f"{self.failure_count} failing run(s) while its run list "
                f"shows {failures}")
        if self.timeout_count != timeouts:
            problems.append(
                f"the {self.role} measurement reports "
                f"{self.timeout_count} timed-out run(s) while its run list "
                f"shows {timeouts}")
        rate = self.result["failure_rate"]
        expected = (round(self.failure_count / self.requested_runs, 6)
                    if self.requested_runs else None)
        if rate != expected:
            problems.append(
                f"the {self.role} measurement reports the failure rate "
                f"{rate!r}, not the {expected!r} its own counts imply")
        return problems

    def defect_problems(self) -> list[str]:
        """Everything a TRUSTWORTHY measurement observed going wrong.

        Its counterpart above answers "can this batch be believed"; this
        answers "did it show anything", and it asks the RUN LIST and the
        PER-CHECK TALLIES rather than the aggregates. Those two are
        genuinely independent — a run can time out after emitting every
        declared check, and a check can go MISSING across a batch whose
        every run PASSED — while the totals are the same fact counted,
        and `aggregate_problems` has already bound them to the run list.
        Reading a total here would be asking the derived value instead
        of its source, and would leave a clause no fixture can isolate.
        """
        problems: list[str] = []
        for run in self.result["runs"]:
            if run["outcome"] != probe_flake.RUN_PASS:
                problems.append(
                    f"the {self.role} measurement's run {run['index']} is "
                    f"{run['outcome']}, not {probe_flake.RUN_PASS}")
        for cid, tally in sorted(self.check_counts.items()):
            for state in (probe_protocol.FAIL, probe_protocol.MISSING):
                if tally.get(state):
                    problems.append(
                        f"the {self.role} measurement's check {cid!r} is "
                        f"{state} in {tally[state]} run(s)")
        return problems

    def missing_targets(self, targets) -> list[str]:
        """The target checks this measurement never observed at all.

        A reproducibly MISSING target is #1437's second qualification
        for a repair, so it is also the second way a batch can fail an
        acceptance gate while sitting at or below X.
        """
        return [cid for cid in targets
                if self.check_counts.get(cid, {}).get(probe_protocol.MISSING)]

    def summary(self) -> dict:
        """The durable summary this measurement contributes to the census.

        References, never copies. The per-run CHECK MAP, the ports, the
        artifact root and the invocation directory stay in the result
        document beside the retained artifacts; what is stored is the
        tally the outcome was judged on and the paths that hold the
        rest.
        """
        result = self.result
        error_run = result.get("error_run")
        return {
            "role": self.role,
            "exit_code": self.exit_code,
            "status": result["status"],
            "commit_sha": result["commit_sha"],
            "timestamp_utc": result["timestamp_utc"],
            "requested_runs": result["requested_runs"],
            "completed_runs": result["completed_runs"],
            "runs": [{"index": run["index"], "outcome": run["outcome"]}
                     for run in result["runs"]],
            "check_counts": copy.deepcopy(result["check_counts"]),
            "failure_count": result["failure_count"],
            "failure_rate": result["failure_rate"],
            "timeout_count": result["timeout_count"],
            "rts_capabilities": result["rts_capabilities"],
            "error": result["error"],
            "error_run_index": (None if error_run is None
                                else error_run["index"]),
            "retained_artifacts": list(result["retained_artifacts"]),
            "census_reference": {
                "cohort_commit_sha": result["commit_sha"],
                "sample_timestamp_utc": result["timestamp_utc"],
            },
        }


class Handoff:
    """One accepted outcome handoff: one probe, one de-flake attempt."""

    def __init__(self, *, attempt: str, summary: str, diagnosis,
                 measurements: dict):
        self.attempt = attempt
        self.summary = summary
        self.diagnosis = diagnosis
        self.measurements = measurements

    @property
    def route(self) -> str:
        return self.diagnosis["route"]

    @property
    def reason(self) -> str:
        return self.diagnosis["reason"]

    @property
    def probe(self) -> str:
        return self.diagnosis["probe"]

    @property
    def targets(self) -> list:
        return list(self.diagnosis["targets"])

    @property
    def acceptable_failures(self) -> int:
        return self.diagnosis["acceptable_failures"]

    @property
    def baseline_sha(self) -> str:
        return self.diagnosis["baseline_sha"]

    @property
    def configuration(self) -> list:
        return copy.deepcopy(self.diagnosis["configuration"])

    @property
    def invocation(self) -> dict:
        return copy.deepcopy(self.diagnosis["invocation"])

    def measurement(self, role: str):
        return self.measurements.get(role)

    def artifacts(self) -> list:
        """Every retained artifact this attempt has, in a stable order.

        Taken from the producer record alone, because the entry gate has
        already REBUILT that field from the per-batch references and
        required the two to be equal — and every declared measurement is
        bound to its reference's retained list exactly. Unioning the
        measurements in again here would add nothing and would quietly
        re-admit whatever the equality check exists to refuse.
        """
        return list(self.diagnosis["retained_artifacts"])


def _registered_probes() -> set:
    return {key for key, _script, _purpose in probe_runner_registry.PROBES}


def require_diagnosis_outcome(document, *, worktrees=(),
                              owned: RouteOwnership) -> dict:
    """#1437's producer record, held to the fields this consumer reads.

    Deliberately not a re-validation of the whole
    `deflake-diagnosis-outcome/v1` document: #1437 owns that record and
    this module does not second-guess the parts it does not use. What is
    checked is that every field this classification rests on is present
    and means what it says.

    `owned` is the ONE part of the gate that differs between epic
    #1426's two sibling consumers, so it is a parameter rather than a
    second copy of everything above it. It is required rather than
    defaulted: a shared contract that carried one consumer's routes as
    its default would answer for that consumer whenever a caller forgot
    to say which routes it owns.
    """
    outcome = _require_object(document, "the diagnosis outcome")
    schema = outcome.get("schema")
    if schema != DIAGNOSIS_OUTCOME_SCHEMA:
        raise HandoffError(
            f"the diagnosis outcome is {schema!r}, expected "
            f"{DIAGNOSIS_OUTCOME_SCHEMA!r}")
    route = outcome.get("route")
    if route not in deflake_diagnosis.ROUTES:
        raise HandoffError(
            f"the diagnosis outcome declares the route {route!r}; the "
            f"declared routes are {', '.join(deflake_diagnosis.ROUTES)}")
    if outcome.get("opens_pull_request"):
        raise HandoffError(
            f"the diagnosis outcome opens a pull request, so it is #1437's "
            f"repair ending and not a non-success this workflow records")
    if not owned.owns(route):
        owner = deflake_diagnosis.ROUTE_OWNER.get(route)
        ending = ROUTE_ENDING.get(route, f"declares the route {route!r}")
        if owner is not None and owner != owned.issue:
            # A well-formed handoff for a route this workflow does not
            # own but a SIBLING does. It is a non-success rather than a
            # malformed input, and it names the owner rather than
            # stubbing it: the siblings in epic #1426 are not each
            # other's prerequisites, so ordering between them must not
            # be able to break either one.
            raise NonSuccess(
                f"the diagnosis {ending}, which is #{owner}'s route: this "
                f"attempt records none of the outcomes this workflow owns "
                f"({', '.join(owned.outcomes)}) and opens no pull request")
        raise HandoffError(
            f"the route {route!r} is not one this workflow records; it hands "
            f"off to {'#%d' % owner if owner else 'nobody'}, and the routes "
            f"this workflow owns are "
            f"{', '.join(sorted(owned.roles))}")
    reason = outcome.get("reason")
    if reason not in deflake_diagnosis.ROUTE_REASONS.get(route, ()):
        raise HandoffError(
            f"the diagnosis outcome declares the reason {reason!r}, which "
            f"the {route!r} route cannot be reached for; its reasons are "
            f"{', '.join(deflake_diagnosis.ROUTE_REASONS.get(route, ()))}")
    probe = outcome.get("probe")
    if probe not in _registered_probes():
        raise HandoffError(
            f"the diagnosis outcome names the probe {probe!r}, which is not "
            f"registered in probe_runner_registry.PROBES")
    # Required rather than defaulted to empty. `targets` drives the
    # MISSING half of the acceptance gate, so a record that lost it
    # would quietly weaken the `no-confident-fix` and
    # `partial-improvement` predicates instead of failing. #1437 always
    # writes the key, empty list and all.
    targets = _require_string_list(outcome.get("targets"),
                                   "the diagnosis outcome's `targets`")
    # The target list and the route are two statements of one fact, and
    # #1437 makes them agree in BOTH directions: `no-target` is what an
    # all-PASS measurement produces, and it refuses that route over a
    # handoff naming targets — while every other route is refused over a
    # handoff naming NONE, because there is then nothing to diagnose. A
    # consumer that let either through would classify against a premise
    # its own producer had rejected: a `no-target` record naming `beta`
    # would earn an advisory de-list from a measurement that observed
    # `beta` going wrong.
    if route == deflake_diagnosis.ROUTE_NO_TARGET and targets:
        raise HandoffError(
            f"the {route!r} route is what an all-PASS measurement produces, "
            f"so its record names no target; this one names "
            f"{', '.join(targets)}")
    if route != deflake_diagnosis.ROUTE_NO_TARGET and not targets:
        raise HandoffError(
            f"the diagnosis outcome names no target check, which is the "
            f"{deflake_diagnosis.ROUTE_NO_TARGET!r} ending and not "
            f"{route!r}; every other route diagnoses at least one observed "
            f"non-PASS check")
    acceptable = outcome.get("acceptable_failures")
    acceptable = delegate_census_grammar(
        lambda: probe_census.require_acceptable_failures(
            acceptable, "the diagnosis outcome's `acceptable_failures`"),
        "the acceptable-failure ceiling")
    baseline_sha = delegate_census_grammar(
        lambda: probe_census.require_commit_identity(
            outcome.get("baseline_sha"),
            "the diagnosis outcome's `baseline_sha`"),
        "the baseline commit")
    artifacts = _require_string_list(
        outcome.get("retained_artifacts"),
        "the diagnosis outcome's `retained_artifacts`")
    for path in artifacts:
        require_artifact_reference(
            path, "a retained artifact of the diagnosis outcome",
            worktrees=worktrees)
    expected_checks = _require_string_list(
        outcome.get("handoff", {}).get("expected_checks")
        if isinstance(outcome.get("handoff"), dict) else None,
        "the diagnosis outcome's `handoff`.expected_checks")
    if not expected_checks:
        raise HandoffError(
            "the diagnosis outcome's `handoff`.expected_checks is empty; a "
            "measurement that declared no check reported nothing")
    descriptor = _require_descriptor_record(
        outcome["handoff"].get("expected_descriptor"),
        "the diagnosis outcome's `handoff`.expected_descriptor",
        expected_checks)
    stray = [cid for cid in targets if cid not in expected_checks]
    if stray:
        raise HandoffError(
            f"the diagnosis outcome targets {', '.join(stray)}, which its "
            f"own expected descriptor {expected_checks} does not declare; "
            f"the targets are the measurement's own non-PASS identifiers, "
            f"so one it never declared is not among them")
    # The input identity #1437 states twice, reconciled once.
    require_input_identity(
        outcome.get("handoff"), "the diagnosis outcome's `handoff`",
        probe=probe, baseline_sha=baseline_sha,
        acceptable_failures=acceptable, targets=targets)
    # WHICH measurement #1437 judged for each batch it ran. A route that
    # ran no such batch states `null` rather than dropping the key, so an
    # absent reference is a fact about the invocation and not a gap.
    references = {
        ROLE_HANDOFF: _batch_reference(
            outcome.get("handoff"), "the diagnosis outcome's `handoff`",
            worktrees=worktrees),
        ROLE_BASELINE: _batch_reference(
            outcome.get("baseline"), "the diagnosis outcome's `baseline`",
            worktrees=worktrees),
        ROLE_VERIFICATION: _batch_reference(
            outcome.get("verification"),
            "the diagnosis outcome's `verification`", worktrees=worktrees),
    }
    # #1437 builds its top-level `retained_artifacts` by accumulating
    # every batch it ran, in role order, deduplicated — so the field is
    # DERIVED and the consumer can rebuild it instead of believing it.
    # Validating each path individually only asks whether each names a
    # legal place; it never asks whether the list is the one the
    # invocation actually produced, so an unrelated directory appended
    # here alone would be stored as this attempt's evidence.
    union: list = []
    for role in ROLES:
        reference = references.get(role)
        if reference is None:
            continue
        for path in reference["retained_artifacts"]:
            if path not in union:
                union.append(path)
    if artifacts != union:
        raise HandoffError(
            f"the diagnosis outcome's `retained_artifacts` is {artifacts} "
            f"where the batches it references retained {union}; that list "
            f"is the ordered, deduplicated union of every batch the "
            f"invocation ran, so a set that is not it names evidence this "
            f"attempt does not have, or hides evidence it does")
    return {
        "route": route,
        "reason": reason,
        "probe": probe,
        "targets": targets,
        "acceptable_failures": acceptable,
        "baseline_sha": baseline_sha,
        "retained_artifacts": artifacts,
        "expected_checks": expected_checks,
        "expected_descriptor": descriptor,
        "references": references,
        "configuration": require_configuration(
            outcome.get("configuration"),
            "the diagnosis outcome's `configuration`"),
        "invocation": require_invocation_identity(
            outcome.get("handoff"), "the diagnosis outcome's `handoff`"),
    }


# Every field of #1437's per-batch reference that a result document also
# reports, so a declared measurement can be held to ALL of them. The
# commit and the instant alone are not an identity: two batches of one
# probe at one commit differ in where they wrote, and
# `probe_flake.new_invocation_dir` stamps a fresh directory per
# invocation precisely so that they do.
REFERENCE_FIELDS = ("commit_sha", "timestamp_utc", "artifact_root",
                    "invocation_dir", "retained_artifacts")


def _batch_reference(section, what: str, *, worktrees=()):
    """The identity #1437 recorded for one batch, or None if it ran none.

    #1437's outcome document carries a REFERENCE per batch rather than
    the document itself, so this is what says WHICH measurement it
    judged. A malformed reference is a malformed producer record.
    """
    if section is None:
        return None
    reference = _require_object(section, what)
    commit = delegate_census_grammar(
        lambda: probe_census.require_commit_identity(
            reference.get("commit_sha"), f"{what}'s `commit_sha`"),
        f"{what}'s commit")
    stamp = reference.get("timestamp_utc")
    delegate_census_grammar(lambda: probe_census.parse_timestamp(
        stamp, f"{what}'s `timestamp_utc`"), f"{what}'s timestamp")
    artifacts = _require_string_list(
        reference.get("retained_artifacts"),
        f"{what}'s `retained_artifacts`")
    for path in artifacts:
        require_artifact_reference(path, f"a retained artifact of {what}",
                                   worktrees=worktrees)
    return {
        "commit_sha": commit,
        "timestamp_utc": stamp,
        "artifact_root": require_artifact_reference(
            reference.get("artifact_root"), f"{what}'s `artifact_root`",
            worktrees=worktrees),
        "invocation_dir": require_artifact_reference(
            reference.get("invocation_dir"), f"{what}'s `invocation_dir`",
            worktrees=worktrees),
        "retained_artifacts": artifacts,
    }


def require_measurement(entry, *, probe: str, seen: set,
                        worktrees=()) -> Measurement:
    """One declared measurement, bound to its own exit code."""
    section = _require_object(entry, "a declared measurement")
    role = section.get("role")
    if role not in ROLES:
        raise HandoffError(
            f"a declared measurement's role is {role!r}; the roles are "
            f"{', '.join(ROLES)}")
    if role in seen:
        raise HandoffError(
            f"the handoff declares the {role!r} measurement twice; one "
            f"attempt has one of each")
    exit_code = section.get("exit_code")
    if isinstance(exit_code, bool) or not isinstance(exit_code, int):
        raise HandoffError(
            f"the {role} measurement's `exit_code` must be an integer, got "
            f"{exit_code!r}")
    if exit_code not in EXIT_CONTRACT:
        raise HandoffError(
            f"the {role} measurement exited {exit_code}, which is not one of "
            f"tools/probe_flake.py's exits "
            f"({', '.join(str(code) for code in sorted(EXIT_CONTRACT))})")
    expected_status = EXIT_CONTRACT[exit_code]
    result = section.get("result")
    if expected_status is None:
        if result is not None:
            raise HandoffError(
                f"the {role} measurement exited {exit_code}, which is caught "
                f"before any document is rendered, so it wrote no result "
                f"document and one here did not come from that run")
        return Measurement(role, exit_code, None)
    if result is None:
        raise HandoffError(
            f"the {role} measurement exited {exit_code}, which writes a "
            f"result document, but the handoff carries none")
    # #1437's CANONICAL result gate, not just the census validator it
    # starts with. `probe_census.validate_result` owns declared shape
    # and #1493's cross-field invariants; `require_result` adds the
    # rules that make a document one `probe_flake.measure` could have
    # written — run indices, the artifact TOPOLOGY, and the retention
    # pairing in particular. `measure` deletes a run's directory the
    # moment it passes and keeps every unsuccessful one, so a non-PASS
    # run with a null `artifact_dir` is producer-impossible, and a
    # `no-confident-fix` recorded from one would be a failure nobody can
    # diagnose stored as the evidence for a diagnosis.
    try:
        deflake_diagnosis.require_result(
            result, f"the {role} measurement's result document")
    except deflake_diagnosis.HandoffError as error:
        raise HandoffError(str(error)) from None
    if result["probe"] != probe:
        raise HandoffError(
            f"the {role} measurement measured {result['probe']!r}, not the "
            f"diagnosed probe {probe!r}")
    if result["status"] != expected_status:
        raise HandoffError(
            f"the {role} measurement exited {exit_code} while its document "
            f"reports status {result['status']!r}, which that exit cannot "
            f"produce (expected {expected_status!r})")
    for path in result["retained_artifacts"]:
        require_artifact_reference(
            path, f"a retained artifact of the {role} measurement",
            worktrees=worktrees)
    for field in ("artifact_root", "invocation_dir"):
        require_artifact_reference(
            result[field], f"the {role} measurement's `{field}`",
            worktrees=worktrees)
    return Measurement(role, exit_code, result)


def _bind_to_producer(measurement: Measurement, diagnosis: dict) -> None:
    """The declared measurement IS the one #1437 judged, and no other.

    Binding on the probe alone would admit any well-formed batch of the
    same probe: a result taken at another commit, or another instant,
    could be supplied under a diagnosis that judged a different one, and
    the census would then store two conflicting accounts of one attempt.
    So each measurement is held to EVERY field of the producer record's
    reference for its role that a result document also reports — the
    commit, the instant, the artifact root, the invocation directory and
    the ordered retained artifacts. The commit and the instant alone are
    not an identity: two batches of one probe at one commit differ in
    where they wrote, which is exactly why
    `probe_flake.new_invocation_dir` stamps a fresh directory per
    invocation, and a substitute agreeing on the first two would still
    hand the census another batch's artifacts as this attempt's
    evidence.

    The pre-fix roles are additionally held to the `baseline_sha` the
    census row is about to record — an independent statement, because a
    producer record whose reference and `baseline_sha` disagreed would
    satisfy either one alone.

    A role the producer ran no batch for has a `null` reference, and a
    measurement supplied for it describes work the invocation did not
    do.
    """
    if measurement.result is None:
        # Exits 2 and 3 wrote no document, so there is nothing to bind.
        # `trustworthiness_problems` is what refuses them, as an
        # operational error rather than a malformed handoff.
        return
    reference = diagnosis["references"].get(measurement.role)
    if reference is None:
        raise HandoffError(
            f"the diagnosis outcome records no {measurement.role} batch, so "
            f"a {measurement.role} measurement here describes work the "
            f"invocation did not do")
    result = measurement.result
    for field in REFERENCE_FIELDS:
        if result[field] != reference[field]:
            raise HandoffError(
                f"the {measurement.role} measurement reports {field} "
                f"{result[field]!r} while the diagnosis outcome's "
                f"{measurement.role} reference names "
                f"{reference[field]!r}, so it is not the measurement that "
                f"diagnosis judged")
    if measurement.role in PRE_FIX_ROLES:
        if result["commit_sha"] != diagnosis["baseline_sha"]:
            raise HandoffError(
                f"the {measurement.role} measurement was taken at "
                f"{result['commit_sha']!r}, not at the diagnosis outcome's "
                f"baseline commit {diagnosis['baseline_sha']!r}; a pre-fix "
                f"measurement of another commit is not this attempt's "
                f"evidence")


def require_worktree_boundary(value, what: str) -> str:
    """One declared comparison worktree, as a boundary rather than a label.

    #1437 canonicalises both declared worktrees and requires each batch
    to have run inside the one its section names, so a real record
    carries the spelling `Path.resolve` produces. That rule is restated
    here — the producer exposes it publicly only through
    result-document validation — because this is the value the
    containment check is made AGAINST: a relative, `..`-bearing or
    symlinked spelling would compare as somewhere other than the place
    it names, and the boundary would quietly stop covering it.

    What this cannot establish is that the named directory ever WAS a
    worktree. Nothing else in the record identifies those directories,
    and by the time an outcome is recorded both have usually been
    removed, so a plausible substitute is indistinguishable from a
    truthful declaration. The declared worktrees are therefore an
    ADDITIONAL boundary the record supplies about itself: they can only
    ever add refusals, never lift one, and the live registered worktrees
    and the primary checkout — which no document can edit — are what
    the guarantee actually rests on.
    """
    text = _require_usable_path(
        _require_text(value, what, limit=4096), what)
    if not Path(text).is_absolute():
        raise HandoffError(
            f"{what} is the relative path {text!r}; a containment boundary "
            f"has to name one place, and a relative one names a different "
            f"place from every directory it is read in")
    resolved = str(Path(text).resolve())
    if resolved != text:
        raise HandoffError(
            f"{what} is {text!r}, which is not the spelling `Path.resolve` "
            f"produces ({resolved!r}); #1437 canonicalises both declared "
            f"worktrees, and a boundary compared in one spelling while the "
            f"paths it bounds are written in another covers nothing")
    return text


def declared_worktrees(document) -> list:
    """The comparison worktrees the producer record names.

    Collected BEFORE anything is admitted, and kept beside the live
    registered ones, because the workflow removes or hands off both
    comparison worktrees when it finishes: by the time an outcome is
    recorded the paths it correctly names may no longer be registered,
    and an artifact that sat inside one was still inside a worktree when
    it was written.

    REQUIRED of every batch the record says it ran, for the same reason:
    a section that declared no worktree would silently contribute no
    boundary, and the artifacts of a removed worktree would then be
    stored unbounded. Both are held to #1437's own rule that neither may
    contain the other, since two labels for one place are not two
    states.
    """
    trees = []
    for section in ("baseline", "verification"):
        record = document.get(section) if isinstance(document, dict) else None
        if not isinstance(record, dict):
            continue
        trees.append(require_worktree_boundary(
            record.get("worktree"),
            f"the diagnosis outcome's {section} `worktree`"))
    if len(trees) == 2:
        first, second = (Path(tree) for tree in trees)
        if (first == second or first in second.parents
                or second in first.parents):
            raise HandoffError(
                f"the diagnosis outcome declares the comparison worktrees "
                f"{trees[0]} and {trees[1]}, which are not two separate "
                f"states; the clean comparison worktree stays at the "
                f"baseline commit and the repair lives in its own")
    return trees


def _require_descriptor_record(value, what: str, expected_checks) -> list:
    """#1437's WHOLE expected descriptor, identifiers and labels.

    Required, and required to agree with the identifier list beside it:
    the record states the descriptor twice, and two statements free to
    disagree establish neither.
    """
    if not isinstance(value, list) or not value:
        raise HandoffError(
            f"{what} must be the handoff's ordered descriptor, got "
            f"{type(value).__name__}")
    descriptor = []
    for position, entry in enumerate(value):
        where = f"{what}[{position}]"
        record = _require_object(entry, where)
        if set(record) != {"id", "label"}:
            raise HandoffError(
                f"{where} carries {sorted(record)}, not exactly `id` and "
                f"`label`")
        descriptor.append({
            "id": _require_text(record["id"], f"{where}.id", limit=128),
            "label": _require_text(record["label"], f"{where}.label",
                                   limit=4096),
        })
    ids = [entry["id"] for entry in descriptor]
    if ids != list(expected_checks):
        raise HandoffError(
            f"{what} declares {ids} where the record's own "
            f"`expected_checks` is {list(expected_checks)}; one descriptor "
            f"stated twice cannot be two descriptors")
    return descriptor


def _require_one_descriptor(measurements: dict, diagnosis: dict) -> None:
    """Every declared measurement reports the contract the record names.

    A batch's descriptor is what it reports against, and
    `_bind_to_producer` cannot see it: a result can keep its probe, its
    targets, its commit, its instant and every artifact path while
    swapping or relabelling an unrelated declared check, and #1437
    rejects exactly that drift rather than routing it anywhere.

    Held to the RECORD's own ordered descriptor rather than to the other
    supplied measurements, through #1437's `require_descriptor` so
    identifiers, order AND labels are compared by one definition. The
    difference matters: `cannot-reproduce` and `no-confident-fix`
    normally supply a single baseline, so a comparison between
    measurements has nothing to compare it against, and a self-
    consistent handoff that relabelled a check would record a stable
    outcome — a de-list recommendation among them — for a different
    asserted check.
    """
    expected = diagnosis["expected_descriptor"]
    for role in ROLES:
        measurement = measurements.get(role)
        if measurement is None or measurement.result is None:
            continue
        try:
            deflake_diagnosis.require_descriptor(
                measurement.result, expected,
                f"the {role} measurement's result document")
        except deflake_diagnosis.HandoffError as error:
            raise HandoffError(str(error)) from None


def require_handoff(document, *, worktrees=(), primary=None,
                    owned: RouteOwnership) -> Handoff:
    """One `deflake-outcome-handoff/v1`, or the refusal that names why.

    `owned` selects which of #1437's routes the caller answers; every
    other rule below is the same for both of epic #1426's consumers.
    """
    envelope = _require_object(document, "the outcome handoff")
    schema = envelope.get("schema")
    if schema != HANDOFF_SCHEMA:
        raise HandoffError(
            f"the outcome handoff is {schema!r}, expected {HANDOFF_SCHEMA!r}")
    attempt = _require_identity(envelope.get("attempt"),
                                "the handoff's `attempt` identity")
    summary = _require_text(envelope.get("summary"),
                            "the handoff's diagnostic or attempted-fix "
                            "`summary`", limit=MAX_SUMMARY)
    trees = [tree for tree in
             list(worktrees) + declared_worktrees(
                 envelope.get("diagnosis_outcome"))
             + ([primary] if primary else [])
             if tree]
    diagnosis = require_diagnosis_outcome(envelope.get("diagnosis_outcome"),
                                          worktrees=trees, owned=owned)
    entries = envelope.get("measurements")
    if not isinstance(entries, list) or not entries:
        raise HandoffError(
            "the handoff must declare at least one measurement, each with "
            "the exit code its harness invocation returned")
    measurements: dict = {}
    for entry in entries:
        measurement = require_measurement(entry, probe=diagnosis["probe"],
                                          seen=set(measurements),
                                          worktrees=trees)
        measurements[measurement.role] = measurement
    roles = owned.roles[diagnosis["route"]]
    for role in roles["required"]:
        if role not in measurements:
            raise HandoffError(
                f"the {diagnosis['route']!r} route rests on its {role} "
                f"measurement, which the handoff does not declare")
    # The route's own policy first, then the identity of what it was
    # given: "this route runs no verification batch" is the more
    # actionable sentence, and it is true of the ROUTE whatever the
    # producer record happens to reference.
    for role in roles["forbidden"]:
        if role in measurements:
            raise HandoffError(
                f"the {diagnosis['route']!r} route runs no {role} batch, so "
                f"a {role} measurement here describes work the route did not "
                f"do")
    for measurement in measurements.values():
        _bind_to_producer(measurement, diagnosis)
    _require_one_descriptor(measurements, diagnosis)
    if envelope.get("unmet_condition") is not None:
        raise HandoffError(
            "which #1437 acceptance condition a partial improvement failed "
            "is DERIVED from the diagnosis outcome's own `reason`, so the "
            "handoff may not supply one; a stored condition nobody checked "
            "would be the one field of the record that could say anything "
            "at all")
    return Handoff(attempt=attempt, summary=summary,
                   diagnosis=diagnosis, measurements=measurements)


# ==========================================================================
# The shared reproduction predicate
# ==========================================================================

def require_reproduced(handoff: Handoff, baseline: Measurement) -> None:
    """The baseline reproduced the pattern the targets were named from.

    Shared because BOTH of epic #1426's consumers ask it: #1437 asks it
    of every route past the `cannot-reproduce` fork, and neither the
    `production-defect` route nor the two stable outcomes reached past
    that fork are exempt. A private copy in either consumer would be a
    second reading of one producer rule.

    #1437 asks it in two independent parts, and a consumer that asked
    only the first would persist a diagnosis its own producer refuses:

    * the batch is over X or lost a target, which is what makes it a
      reproduction at all; and
    * at least one TARGET was actually non-PASS. Failures confined to
      unrelated checks satisfy the aggregate while demonstrating nothing
      about the checks under diagnosis — `evaluate` calls that
      `cannot-reproduce` precisely because the pattern was not
      reproduced.

    Both are re-derived from the baseline's own document, the second
    through #1437's `non_pass_ids` so "non-PASS" keeps one definition.
    """
    missing = baseline.missing_targets(handoff.targets)
    if (baseline.failure_count <= handoff.acceptable_failures
            and not missing):
        raise NonSuccess(
            f"the baseline observed {baseline.failure_count} failure(s) "
            f"against an acceptable ceiling of "
            f"{handoff.acceptable_failures} out of "
            f"{baseline.requested_runs} and left no target check MISSING, "
            f"so it reproduced nothing to attribute; that is the "
            f"{deflake_diagnosis.ROUTE_CANNOT_REPRODUCE!r} evidence")
    observed = set(deflake_diagnosis.non_pass_ids(baseline.result))
    hit = [cid for cid in handoff.targets if cid in observed]
    if not hit:
        raise NonSuccess(
            f"the baseline never observed the target check(s) "
            f"{', '.join(handoff.targets)} as FAIL or MISSING, so it did "
            f"not reproduce the pattern the targets were identified from; "
            f"failures somewhere else are the "
            f"{deflake_diagnosis.ROUTE_CANNOT_REPRODUCE!r} evidence for these targets")


# ==========================================================================
# The clock, and the one field a retry may reuse
# ==========================================================================

def utc_now() -> str:
    return datetime.now(timezone.utc).strftime(probe_census.TIMESTAMP_FORMAT)


def reuse_stored_timestamp(candidate: dict, stored: dict) -> dict:
    """The one field of a rebuilt record that cannot be derived again.

    Every field of an outcome record comes from the handoff except
    `timestamp_utc`, which comes from a clock — and a clock reads
    differently on a retry. Idempotency is the WHOLE record, so a retry
    that stamped itself anew would present one attempt identity carrying
    two different records and be refused as a conflict instead of
    recognized as the resume it is.

    `probe_census.record_outcome_installed` calls this INSIDE its locked
    transaction, and only when the attempt is already stored. That
    placement is the point: two concurrent invocations of one new
    attempt serialize on the census lock, so the second builds its
    record against what the first actually committed rather than against
    a snapshot taken before it. Nothing else is copied across — every
    other difference is a real one, and `ingest_outcome` still refuses
    it.
    """
    instant = stored.get("timestamp_utc")
    if not isinstance(instant, str):
        return candidate
    return dict(candidate, timestamp_utc=instant)
