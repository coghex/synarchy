#!/usr/bin/env python3
"""The `deflake-outcome-handoff/v1` envelope GRAMMAR, and nothing above it.

One of four internal owners behind `tools/deflake_handoff.py`, which
stays the stable public import façade for epic #1426's two consumers
(`tools/deflake_outcome.py` #1439 and `tools/deflake_issue.py` #1438).
Read that module's docstring for the contract narrative — why each rule
below exists is stated there, once, for both consumers.

What this owns is the vocabulary every other owner is written in: the
schema identifiers, the measurement-role names, what each of #1437's
route endings IS, `RouteOwnership`, `tools/probe_flake.py`'s exit
contract, the two exception classifications, the shared limits, and the
primitive object/text/identity/list/NUL/usable-path grammar the
document-supplied configuration, input identity, invocation identity and
artifact references are parsed with.

It is a LEAF WITHIN the handoff family: it imports no other handoff
owner, neither consumer, and not the façade. It is deliberately not a
leaf of the repository — the upstream rules it applies are called rather
than copied. `deflake_contract.require_path`, `.inside_any_worktree`
and `.require_manifest`, and `probe_census.require_commit_identity` and
`.require_acceptable_failures`, each stay one definition owned upstream;
what happens here is that their refusals are re-raised as this family's
own `HandoffError`, so a caller never sees a producer's exception type
escaping from a consumer it did not invoke.

    python3 tools/test_deflake_diagnosis.py       # the deterministic gate
"""
from __future__ import annotations

import os
import sys
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import deflake_contract  # noqa: E402
import probe_census  # noqa: E402
import probe_flake  # noqa: E402

HANDOFF_SCHEMA = "deflake-outcome-handoff/v1"
# The producer record this consumes, named from the producer rather than
# restated: a second copy that drifted would accept a document #1437
# never writes.
DIAGNOSIS_OUTCOME_SCHEMA = deflake_contract.OUTCOME_SCHEMA

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
# `deflake_contract.ROUTE_OWNER` rather than from a second table here.
ROUTE_ENDING = {
    deflake_contract.ROUTE_NO_TARGET:
        "observed no non-PASS check at all",
    deflake_contract.ROUTE_CANNOT_REPRODUCE:
        "could not reproduce the failure under the handoff's own condition",
    deflake_contract.ROUTE_PRODUCTION_DEFECT:
        "identifies a production-code or shipped-script defect",
    deflake_contract.ROUTE_NO_CONFIDENT_FIX:
        "reproduced the failure and established no one bounded probe-side "
        "repair",
    deflake_contract.ROUTE_PARTIAL_IMPROVEMENT:
        "measurably improved the failure count without passing the "
        "acceptance gate",
    deflake_contract.ROUTE_REPAIR:
        "is #1437's confidently diagnosed repair",
    deflake_contract.ROUTE_HANDOFF_REJECTED:
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
    than `OSError`, and `deflake_contract.inside_any_worktree` resolves
    every form it compares — so a NUL-bearing string would sail past the
    absoluteness test, name no filesystem location for the containment
    check to find, and be stored in the census as an artifact reference
    or an invocation directory.

    `deflake_contract.require_path` is that module's own rule for
    exactly this, applied at every point a document-supplied path is
    first read. Calling it keeps one definition of "a path a recorded
    measurement could have used"; its refusal is re-raised as this
    module's own, so a caller never sees the producer's exception type
    escaping from a consumer it did not invoke.
    """
    try:
        return deflake_contract.require_path(value, what)
    except deflake_contract.HandoffError as error:
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
    tree = deflake_contract.inside_any_worktree(text, worktrees)
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
        manifest = deflake_contract.require_manifest(value, what)
    except deflake_contract.HandoffError as error:
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
