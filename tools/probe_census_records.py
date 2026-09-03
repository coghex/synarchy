#!/usr/bin/env python3
"""The census model: every pure transformation of a census document (#2131).

One level above `probe_census_contract.py`, which says what a document
IS, and one level below `probe_census_storage.py`, which is the only
module that puts one on disk. Everything here is a function from a
document to a document — or to a report about one — and none of it opens
a file, resolves a worktree, takes a lock, parses an argument or prints
anything. That is what lets the storage owner state its transaction as
"read, validate, apply ONE of these, serialize, check, replace": the
mutation in the middle can be reasoned about on its own because it
cannot reach the disk.

What it owns:

* the empty record and the manifest: `empty_census`, `build_manifest`,
  `render_manifest`'s stable serialization, and `validate_manifest`'s
  drift report against the live registry;
* #1430's acceptable-failure policy as data — the X predicate, the
  per-record problems, the invariants, `tolerance_state` and
  `policy_sample`;
* migration across `probe-census/v1` through `/v5`, and
  `reconcile_inventory` against the live registry;
* target lookup and selection: `find_entry`, `target_row`,
  `result_target`, `find_claim`, `find_outcome`;
* the ingestion transformations — `summarize_sample`,
  `summarize_attempt`, `ingest_result`, `ingest_claim`, `ingest_outcome`
  — and the policy and deferral setters `set_policy` and `set_deferral`.

`KEEP` lives here because it is a parameter of those last two and means
nothing anywhere else; `TOUCH_ANY`, which is part of the WRITE
transaction's contract, lives with storage.

This module has no CLI and is not a gate of its own. Every command is
still `python3 tools/probe_census.py`.
"""
from __future__ import annotations

import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ci_probes  # noqa: E402
import probe_flake  # noqa: E402
import probe_runner_registry  # noqa: E402
from probe_census_contract import (  # noqa: E402
    ACCEPTED_STATUS, CENSUS_SCHEMA, CI_ELIGIBLE, CensusError,
    DEFAULT_ACCEPTABLE_FAILURES, MANUAL_ONLY, MAX_ACCEPTABLE_FAILURES,
    MIGRATABLE_SCHEMAS, MIN_ACCEPTABLE_FAILURES, POLICY_RUN_COUNT, SEED_SCHEMA,
    TOLERANCE_ACCEPTABLE, TOLERANCE_NOT_COMPARABLE, TOLERANCE_OVER,
    _reject_non_finite, cohort_statistic, require_measurement_semantics,
)


# Distinguishes "leave this policy field alone" from "clear it to null".
KEEP = object()


# ==========================================================================
# The record
# ==========================================================================
def classification(key: str) -> str:
    """The authoritative CI classification, read from `tools/ci_probes.py`."""
    return CI_ELIGIBLE if key in ci_probes.CI_ELIGIBLE else MANUAL_ONLY


def empty_census() -> dict:
    """A census record that has never been measured.

    Its policy is not empty: X starts at `DEFAULT_ACCEPTABLE_FAILURES`
    (#1430), because "this probe must pass every run" is the position
    every probe holds until someone writes down why it should not.
    """
    return {
        "acceptable_failures": DEFAULT_ACCEPTABLE_FAILURES,
        "acceptable_failures_justification": None,
        "estimated_worst_case_seconds": None,
        "current": None,
        "history": [],
        "attempts": [],
        "claims": [],
        "outcomes": [],
        "deferred": None,
    }


def build_manifest() -> dict:
    """The census the live registry currently implies, with empty records."""
    return {
        "schema": CENSUS_SCHEMA,
        "probes": [
            {
                "key": key,
                "script": script,
                "classification": classification(key),
                "protocol": probe_flake.protocol_status(key),
                "census": empty_census(),
            }
            for key, script, _purpose in probe_runner_registry.PROBES
        ],
    }


def render_manifest(manifest: dict | None = None,
                    what: str = "the census") -> str:
    """The one deterministic, byte-stable serialization of a census.

    `sort_keys` is what makes the bytes a pure function of the content:
    the record blocks in #1428 enumerate required keys and their empty
    values, never a serialized key order.

    `allow_nan=False` is the outbound half of `_reject_non_finite`: no
    candidate may be written with a `NaN` or `Infinity` literal, which
    every other JSON reader would reject. The explicit walk runs first
    only to name the offending path, which the encoder's own message
    does not.
    """
    document = manifest if manifest is not None else build_manifest()
    _reject_non_finite(document, what)
    try:
        return json.dumps(document, indent=2, sort_keys=True,
                          allow_nan=False) + "\n"
    except (TypeError, ValueError) as error:
        raise CensusError(f"census is not serializable as JSON: {error}") from None


def validate_manifest(manifest) -> list[str]:
    """Every disagreement between `manifest`'s INVENTORY and the registry.

    Inventory drift only: a missing, duplicate or extra entry, and any
    row whose script, classification or protocol status disagrees with
    `probe_runner_registry.PROBES`, `tools/ci_probes.py` and
    `probe_flake.PROTOCOL_PROBES`. Each row's `census` field is
    deliberately never inspected here: record SHAPE is the declared
    schema's, which `--validate` applies separately, and the two answer
    different questions about the same file. A `probe-census/v1`
    document is reported here as schema drift and is NOT migrated as a
    side effect; `--seed` is the only operation that migrates.
    """
    problems: list[str] = []
    if not isinstance(manifest, dict):
        return [f"manifest must be a JSON object, got {type(manifest).__name__}"]
    schema = manifest.get("schema")
    if schema != CENSUS_SCHEMA:
        problems.append(
            f"manifest schema is {schema!r}, expected {CENSUS_SCHEMA!r}")
    entries = manifest.get("probes")
    if not isinstance(entries, list):
        return problems + ["manifest `probes` must be a list"]

    expected = {key: (script, classification(key), probe_flake.protocol_status(key))
                for key, script, _purpose in probe_runner_registry.PROBES}
    seen: set[str] = set()
    for position, entry in enumerate(entries):
        if not isinstance(entry, dict):
            problems.append(f"entry {position} is not an object: {entry!r}")
            continue
        key = entry.get("key")
        if not isinstance(key, str):
            problems.append(f"entry {position} has no string `key`: {entry!r}")
            continue
        if key in seen:
            problems.append(f"duplicate entry for probe {key!r}")
            continue
        seen.add(key)
        if key not in expected:
            problems.append(
                f"extra entry {key!r}: not registered in probe_runner_registry.PROBES")
            continue
        script, expected_class, expected_protocol = expected[key]
        if entry.get("script") != script:
            problems.append(
                f"probe {key!r}: manifest script {entry.get('script')!r} "
                f"disagrees with the registry ({script!r})")
        if entry.get("classification") != expected_class:
            problems.append(
                f"probe {key!r}: manifest classification "
                f"{entry.get('classification')!r} disagrees with "
                f"tools/ci_probes.py ({expected_class!r})")
        if entry.get("protocol") != expected_protocol:
            problems.append(
                f"probe {key!r}: manifest protocol status "
                f"{entry.get('protocol')!r} disagrees with the in-repo "
                f"registry ({expected_protocol!r})")
    for key in expected:
        if key not in seen:
            problems.append(f"missing entry for registered probe {key!r}")
    return problems


# ==========================================================================
# The acceptable-failure policy (#1430)
# ==========================================================================
#
# #1428 stored a nullable X while the policy was still being chosen.
# This chooses it, and closes the null: every registered probe has an
# acceptable-failure count, X=0 is the default, and X above 0 is a
# maintainer's written decision rather than a bug someone gave up on.
#
# The three rules are CROSS-FIELD, which is why they are code and not
# schema (#1493's split): a justification is a different field from the
# X it explains, and a row's classification is a different field again.
# The declared schema still owns X's own type and range.
#
# They are applied ASYMMETRICALLY, unlike the schema and the cross-field
# invariants, which bracket every mutation. A census written before this
# policy existed holds null Xs, and `--seed` has to be able to READ one
# in order to initialize them — so these run on the CANDIDATE side of
# every mutation and on `--validate`, never on the stored side. What no
# operation may do is INSTALL a policy-invalid census.
#
# X and its justification stay independent in the #1479 sense: changing
# X never rewrites the stored text, and X=0 may keep one. The rule added
# here is one-directional — X above 0 REQUIRES a reason to be there.
def _is_x(value) -> bool:
    """A real integer, for policy purposes.

    `bool` is an `int` subclass, so `True` would otherwise pass as an X
    of one and `False` as the default. Neither is a count.
    """
    return isinstance(value, int) and not isinstance(value, bool)


def require_acceptable_failures(value, what: str) -> int:
    """`value` as a usable X, or the controlled refusal that names it."""
    if not _is_x(value):
        raise CensusError(
            f"{what} must be an integer from {MIN_ACCEPTABLE_FAILURES} "
            f"through {MAX_ACCEPTABLE_FAILURES}, got {value!r}")
    if not MIN_ACCEPTABLE_FAILURES <= value <= MAX_ACCEPTABLE_FAILURES:
        raise CensusError(
            f"{what} must be an integer from {MIN_ACCEPTABLE_FAILURES} "
            f"through {MAX_ACCEPTABLE_FAILURES}: X is counted out of "
            f"{POLICY_RUN_COUNT} runs, so an X of {POLICY_RUN_COUNT} would "
            f"accept a probe that never passes. Got {value!r}")
    return value


def policy_record_problems(census, live_classification, where,
                           name) -> list[str]:
    """Every acceptable-failure violation in ONE census record.

    `live_classification` is the classification this row is judged
    against; see `policy_invariants` for which one that is.
    """
    value = census.get("acceptable_failures")
    if value is None:
        return [f"at {where}, {name} has no acceptable-failure policy (X is "
                f"null); every probe in the census has one, and "
                f"`python3 tools/probe_census.py --seed` initializes an unset "
                f"X to {DEFAULT_ACCEPTABLE_FAILURES}"]
    try:
        acceptable = require_acceptable_failures(
            value, f"at {where}, {name}'s `acceptable_failures`")
    except CensusError as error:
        # Reported rather than raised: `--validate` names every offending
        # row in one pass, and a malformed stored X is exactly the state
        # that must stay VISIBLE instead of being silently repaired.
        return [str(error)]
    if acceptable == MIN_ACCEPTABLE_FAILURES:
        return []
    problems: list[str] = []
    justification = census.get("acceptable_failures_justification")
    if not isinstance(justification, str) or not justification.strip():
        problems.append(
            f"at {where}, {name} accepts {acceptable} failure(s) out of "
            f"{POLICY_RUN_COUNT} with no stated reason "
            f"({justification!r}); a tolerance without one is "
            f"indistinguishable from a bug someone gave up on, so supply it "
            f"with --justification")
    if live_classification == CI_ELIGIBLE:
        problems.append(
            f"at {where}, {name} accepts {acceptable} failure(s) while it is "
            f"{CI_ELIGIBLE}; CI stops on a single failure, so tolerance is a "
            f"manual-only concept — set X to {MIN_ACCEPTABLE_FAILURES} "
            f"before promoting the probe, rather than letting a promotion "
            f"erase a maintainer's decision")
    return problems


def _registered_keys() -> set:
    return {key for key, _script, _purpose in probe_runner_registry.PROBES}


def policy_invariants(document) -> list[str]:
    """Every acceptable-failure violation in one census.

    A registered probe is judged against its LIVE classification, not
    the column stored beside it, so a promotion is caught by the same
    rule whether or not `--seed` has refreshed the row yet. A row whose
    probe has LEFT the registry is judged by its own stored
    classification, which is the only one it still has; that the row is
    extra at all stays `validate_manifest`'s report.

    A `probe-census/v1` seed carries no census records, so nothing here
    applies to one — its schema drift is `--seed`'s repair.
    """
    registered = _registered_keys()
    problems: list[str] = []
    for position, entry in enumerate(document.get("probes") or []):
        if not isinstance(entry, dict):
            continue
        census = entry.get("census")
        if not isinstance(census, dict):
            continue
        key = entry.get("key")
        known = isinstance(key, str) and key in registered
        live = classification(key) if known else entry.get("classification")
        problems += policy_record_problems(
            census, live, f"$.probes[{position}].census",
            f"probe {key!r}" if key is not None else "this record")
    return problems


def _refuse_policy(problems: list[str], what: str) -> None:
    """A controlled refusal naming the first policy violation, or nothing."""
    if not problems:
        return
    more = ("" if len(problems) == 1
            else f" (and {len(problems) - 1} further violation(s))")
    raise CensusError(
        f"{what} violates the acceptable-failure policy: {problems[0]}{more}")


def tolerance_state(acceptable_failures, requested_runs, completed_runs,
                    failure_count) -> str:
    """Where ONE measurement sits against a record's X.

    Acceptable is `failures <= X` and over tolerance is `failures > X`,
    so an X of one accepts both 10/10 and 9/10 and rejects 8/10.

    The comparison is made ONLY against a single complete
    `POLICY_RUN_COUNT`-run measurement, because that is the basis X is
    stated on. `tools/probe_flake.py` deliberately accepts any positive
    `--runs`, and a measurement with another run count remains valid
    data that this simply does not classify — the alternative would be
    rescaling X, which quietly turns "one failure in ten is acceptable"
    into a rate the maintainer never agreed to.

    The arguments are ONE measurement's own counts, never a cohort's
    pooled totals: two five-run measurements are not a ten-run one, and
    two ten-run measurements are not a twenty-run one. `policy_sample`
    is what picks the measurement this is asked about.

    Nothing here is a judgement about the PROBE: `tools/ci_probes.py`
    classifies probes as flaky, base-failing or scenario-heavy, and a
    breach of this threshold is a fact about one ten-run result.
    """
    if not _is_x(acceptable_failures):
        return TOLERANCE_NOT_COMPARABLE
    if not (MIN_ACCEPTABLE_FAILURES <= acceptable_failures
            <= MAX_ACCEPTABLE_FAILURES):
        return TOLERANCE_NOT_COMPARABLE
    if not _is_x(requested_runs) or requested_runs != POLICY_RUN_COUNT:
        return TOLERANCE_NOT_COMPARABLE
    if not _is_x(completed_runs) or completed_runs != requested_runs:
        return TOLERANCE_NOT_COMPARABLE
    if not _is_x(failure_count) or failure_count < 0:
        return TOLERANCE_NOT_COMPARABLE
    return (TOLERANCE_ACCEPTABLE if failure_count <= acceptable_failures
            else TOLERANCE_OVER)


def policy_sample(cohort):
    """The one measurement in `cohort` the policy is evaluated against.

    The LAST-APPENDED complete `POLICY_RUN_COUNT`-run sample, or None
    when the cohort holds no such measurement.

    Append order is what "newest" already means everywhere in the
    census: commit hashes do not compare, and #1429 picks the
    authoritative cohort the same way. A same-commit cohort accumulates
    runs, so it may hold several measurements or none of the policy's
    size — and pooling them would answer a question nobody asked. Two
    five-run measurements would become a ten-run verdict, and two
    genuine ten-run measurements would total twenty and stop being
    comparable at all, which is exactly backwards.
    """
    if not isinstance(cohort, dict):
        return None
    samples = cohort.get("samples")
    if not isinstance(samples, list):
        return None
    for sample in reversed(samples):
        if not isinstance(sample, dict):
            continue
        requested = sample.get("requested_runs")
        completed = sample.get("completed_runs")
        if (_is_x(requested) and requested == POLICY_RUN_COUNT
                and _is_x(completed) and completed == requested):
            return sample
    return None


# ==========================================================================
# Migration and inventory reconciliation
# ==========================================================================
def _rows(document, what: str) -> list:
    """`document`'s entry list, or a controlled refusal."""
    if not isinstance(document, dict):
        raise CensusError(
            f"{what} must be a JSON object, got {type(document).__name__}")
    entries = document.get("probes")
    if not isinstance(entries, list):
        raise CensusError(f"{what} `probes` must be a list")
    for position, entry in enumerate(entries):
        if not isinstance(entry, dict):
            raise CensusError(f"{what} entry {position} is not an object")
        # Every operation ADDRESSES a row by its key — membership tests,
        # lookups, the preservation comparison. A non-string (or
        # unhashable) key is not a shape complaint, it is a row this
        # tool cannot act on at all.
        if not isinstance(entry.get("key"), str):
            raise CensusError(
                f"{what} entry {position} has no string `key` "
                f"({entry.get('key')!r})")
    return entries


def migrate_document(document) -> dict:
    """A `probe-census/v1` through `/v5` document, as `/v5`.

    Lossless in every step: every existing row survives, in the same
    order, with its existing inventory values, and every policy field,
    cohort, sample, attempt and claim is carried through untouched. The
    v1 migration adds the exact empty census record; it never
    regenerates the inventory from the live registry, because the seeded
    document is migration INPUT. Later migrations add ONLY their empty
    fields: `claims` (#1434), `outcomes` (#1439), then `deferred` here.
    A record that already carries one keeps it exactly as it is, so
    re-migrating an already-migrated document is a no-op rather than a
    truncation. Each addition is tested field by field
    rather than by the document's declared schema string, so a census
    written by an older tool and a census hand-repaired halfway through
    a migration both come out whole.

    A row missing its `census` field, or carrying a non-object one, is
    left exactly as it is rather than repaired: in a v2 through v5
    document that is a declared-schema violation for the validator to
    report, and silently inserting an empty record here would erase the
    evidence on the next write.
    """
    entries = _rows(document, "census")
    schema = document.get("schema")
    if schema not in MIGRATABLE_SCHEMAS:
        raise CensusError(
            f"census schema {schema!r} is not one this tool can read "
            f"(expected one of {MIGRATABLE_SCHEMAS})")
    migrated = []
    for entry in entries:
        row = dict(entry)
        if schema == SEED_SCHEMA and "census" not in row:
            row["census"] = empty_census()
        census = row.get("census")
        if isinstance(census, dict) and not {
                "claims", "outcomes", "deferred"} <= set(census):
            census = dict(census)
            census.setdefault("claims", [])
            census.setdefault("outcomes", [])
            census.setdefault("deferred", None)
            row["census"] = census
        migrated.append(row)
    result = dict(document)
    result["schema"] = CENSUS_SCHEMA
    result["probes"] = migrated
    return result


def _appendable(census: dict, field: str, probe: str) -> list:
    """A stored append-only list, or the refusal that it is not one.

    `history` and `attempts` are appended to by every operation that
    touches a record. A stored value that is not a list is a structural
    error this operation cannot perform through — a controlled refusal,
    not a traceback and not a silent replacement of the stored value.
    """
    value = census.get(field)
    if value is None:
        return []
    if not isinstance(value, list):
        raise CensusError(
            f"probe {probe!r}: `{field}` must be a list to append to, got "
            f"{type(value).__name__}")
    return list(value)


def _archive_current(census: dict, probe: str) -> None:
    """Move the current cohort into history. Never discards one."""
    if census.get("current") is not None:
        census["history"] = _appendable(census, "history", probe) + [
            census["current"]]
        census["current"] = None


def reconcile_inventory(document: dict) -> dict:
    """Refresh the inventory columns without touching accumulated data.

    Migrates first, then: appends a row for every probe registered since
    the document was written, in live registry order and with an empty
    census record; refreshes `script`/`classification`/`protocol` from
    the live registries; and applies PROMOTION — a row the live
    `ci_probes.py` now classifies CI-eligible archives its current
    manual-only cohort into `history` and stops carrying one, keeping
    its history, attempts and policy fields.

    The reverse transition is deliberately asymmetric: a row falling
    back from `ci-eligible` to `manual-only` refreshes its
    `classification` and nothing else — no cohort surgery at all.

    It never deletes a row and never rewrites a census record beyond
    #1430's single policy initialization: a record whose
    `acceptable_failures` is still null gets the default. A probe that
    left `probe_runner_registry.PROBES` keeps its measurements and is reported by
    `validate_manifest` as an extra entry, which is a decision for a
    person.
    """
    result = migrate_document(document)
    live = {key: script for key, script, _purpose in probe_runner_registry.PROBES}
    entries = [dict(entry) for entry in result["probes"]]
    present = {entry.get("key") for entry in entries}
    for entry in entries:
        key = entry.get("key")
        census = entry.get("census")
        if isinstance(census, dict) and census.get(
                "acceptable_failures") is None:
            # #1430's one automatic policy repair: an UNSET X becomes
            # the default. Every non-null X, justification, estimate,
            # cohort, sample and attempt is left exactly as it is, and
            # nothing else about a policy field ever moves here — the
            # preservation gate permits this single transition and
            # refuses any other. It runs before the registry-membership
            # test on purpose, so a row whose probe has left the
            # registry can still be made policy-valid rather than
            # blocking `--seed` forever.
            census = _deep_copy(census)
            census["acceptable_failures"] = DEFAULT_ACCEPTABLE_FAILURES
            entry["census"] = census
        if key not in live:
            continue
        entry["script"] = live[key]
        entry["protocol"] = probe_flake.protocol_status(key)
        entry["classification"] = classification(key)
        if entry["classification"] == CI_ELIGIBLE and isinstance(
                entry.get("census"), dict):
            census = _deep_copy(entry["census"])
            _archive_current(census, key)
            entry["census"] = census
    for key, script, _purpose in probe_runner_registry.PROBES:
        if key in present:
            continue
        entries.append({
            "key": key,
            "script": script,
            "classification": classification(key),
            "protocol": probe_flake.protocol_status(key),
            "census": empty_census(),
        })
    result["probes"] = entries
    return result


# ==========================================================================
# Ingesting a measurement
# ==========================================================================
def _deep_copy(value):
    """A copy no later mutation of the original can reach through."""
    return json.loads(json.dumps(value))


def find_entry(document, probe: str) -> dict | None:
    """The first row named `probe`, or None. A plain lookup."""
    for entry in (document or {}).get("probes") or []:
        if isinstance(entry, dict) and entry.get("key") == probe:
            return entry
    return None


def target_row(document, probe: str, what: str) -> dict:
    """The ONE row an operation may mutate, or a controlled refusal.

    `--probe KEY` and a result document's own `probe` each identify
    exactly one census row. Zero matches is a refusal because there is
    nowhere to append without fabricating inventory `--seed` owns; two
    or more is a refusal because writing the first and leaving the
    second is a silent, undetectable half-update.

    This is the TARGET rule and nothing wider: unrelated missing, added,
    stale or even duplicated rows never refuse a finished measurement —
    only ambiguity about the row being written does.
    """
    matches = [entry for entry in (document or {}).get("probes") or []
               if isinstance(entry, dict) and entry.get("key") == probe]
    if not matches:
        raise CensusError(
            f"probe {probe!r} has no census row; reconcile the inventory "
            f"with `python3 tools/probe_census.py --seed`")
    if len(matches) > 1:
        raise CensusError(
            f"probe {probe!r} has {len(matches)} census rows, so {what} "
            f"cannot say which one to write; the census must name each "
            f"probe exactly once")
    if not isinstance(matches[0].get("census"), dict):
        raise CensusError(
            f"probe {probe!r} has no census record to append to")
    return matches[0]


def result_target(result) -> tuple[str, str]:
    """The two fields `--record` dispatches on, from a VALIDATED result.

    The `probe` whose row it mutates — the result document names its own
    row, which is why `--record` takes no `--probe` and why the durable
    sample omits `probe` — and the `status` that decides whether a
    sample is created. That both are present, are strings, and spell
    something recognized is the declared schema's promise (#1492), which
    `record_result` has already required; an unrecognized `status`
    therefore refuses before anything is logged, rather than being
    logged as a non-accepted attempt.
    """
    return result["probe"], result["status"]


def summarize_sample(result: dict) -> dict:
    """The durable record of one accepted measurement.

    Summarized outcomes and artifact REFERENCES only: the producer-only
    fields `Measurement.to_document` adds — `port`, the per-run `checks`
    map, the `checks` descriptor labels, `error_run`, `artifact_root`
    and `invocation_dir` — are deliberately dropped, and no stdout, no
    protocol stream and no engine log ever enters the census.
    """
    return {
        "timestamp_utc": result["timestamp_utc"],
        "commit_sha": result["commit_sha"],
        "requested_runs": result["requested_runs"],
        "completed_runs": result["completed_runs"],
        "runs": [{"index": run["index"],
                  "outcome": run["outcome"],
                  "elapsed_seconds": run["elapsed_seconds"],
                  "artifact_dir": run["artifact_dir"]}
                 for run in result["runs"]],
        "check_counts": _deep_copy(result["check_counts"]),
        "failure_count": result["failure_count"],
        "failure_rate": result["failure_rate"],
        "timeout_count": result["timeout_count"],
        "worst_elapsed_seconds": result["worst_elapsed_seconds"],
        "total_elapsed_seconds": result["total_elapsed_seconds"],
        "rts_capabilities": result["rts_capabilities"],
        "peak_concurrency": result["peak_concurrency"],
        "retained_artifacts": list(result["retained_artifacts"]),
    }


def summarize_attempt(result: dict, accepted: bool) -> dict:
    """The durable record of one well-formed ingestion attempt."""
    return {
        "timestamp_utc": result["timestamp_utc"],
        "commit_sha": result["commit_sha"],
        "status": result["status"],
        "accepted": accepted,
        "requested_runs": result["requested_runs"],
        "completed_runs": result["completed_runs"],
        "error": result["error"],
        "retained_artifacts": list(result["retained_artifacts"]),
    }


def ingest_result(document: dict, result) -> tuple[dict, str]:
    """`document` with `result` recorded, plus the probe row it touched.

    A `status: "ok"` measurement appends one durable sample and one
    accepted attempt; a well-formed harness error appends only a
    non-accepted attempt and touches neither `current` nor any
    aggregate. An accepted measurement naming the same commit as
    `current` appends to that cohort; a different commit archives the
    complete prior cohort into `history` first — even a commit that
    appeared earlier in history, because append order is what "newest"
    means and two hashes do not compare.

    An accepted measurement must first pass #1429's semantic gate
    (`require_measurement_semantics`); an unusable commit identity,
    timestamp or count refuses and writes nothing at all. The STORED
    current cohort is held to the same standard, because the
    append-or-archive decision reads it: an unusable one refuses the
    ingestion instead of being extended or archived.

    Deliberately NOT idempotent: recording the same document twice
    appends a second sample and a second attempt. The record is
    append-only and this slice introduces no deduplication key.
    """
    probe, status = result_target(result)
    entries = _rows(document, "census")
    target_row(document, probe, "--record")
    accepted = status == ACCEPTED_STATUS
    if accepted:
        # #1429's semantic gate, in front of the transaction: an
        # accepted measurement whose commit identity, timestamp or
        # aggregation counts are unusable refuses outright, logging no
        # attempt either. A harness error is deliberately NOT gated —
        # it contributes to no cohort and no aggregate, and `unknown`
        # provenance is exactly the kind of failure the attempt log
        # exists to retain.
        require_measurement_semantics(result)
    sample = summarize_sample(result) if accepted else None
    attempt = summarize_attempt(result, accepted)
    commit = attempt["commit_sha"]

    rows = [dict(entry) for entry in entries]
    for row in rows:
        if row.get("key") != probe:
            continue
        census = _deep_copy(row["census"])
        if sample is not None:
            current = census.get("current")
            if current is not None:
                # The append-or-archive decision READS the stored
                # cohort, so #1429's semantic checks apply to it too:
                # an unusable one refuses the whole ingestion rather
                # than being silently extended or archived into
                # history. Without this a legacy or hand-edited cohort
                # keyed by (or containing) `unknown` would accept a
                # valid measurement and only fail later, on read.
                cohort_statistic(
                    current, f"probe {probe!r} stored current cohort")
            if not isinstance(current, dict) or current.get("commit_sha") != commit:
                _archive_current(census, probe)
                census["current"] = {"commit_sha": commit, "samples": []}
            census["current"]["samples"] = _appendable(
                census["current"], "samples", probe) + [sample]
        census["attempts"] = _appendable(census, "attempts", probe) + [attempt]
        row["census"] = census
        break
    updated = dict(document)
    updated["probes"] = rows
    return updated, probe


# ==========================================================================
# Recording a claim acquisition (#1434)
# ==========================================================================
# A CLAIM is not a measurement and does not live with the measurements.
# `attempts` is the result-ingestion log and is deliberately
# non-idempotent — the same result recorded twice is two attempts,
# because a repeated ingestion is a real event worth seeing. An
# acquisition is the opposite: it is identified by its acquisition
# TOKEN, one lock acquisition produces exactly one record however many
# times the recorder retries, and a second acquisition of the same probe
# is a different token and a different record. Overloading `attempts`
# would have to give up one of those two contracts, so `claims` is its
# own collection.
def find_claim(census, token: str):
    """The stored claim carrying `token`, or None. A plain lookup."""
    for claim in (census or {}).get("claims") or []:
        if isinstance(claim, dict) and claim.get("token") == token:
            return claim
    return None


def ingest_claim(document: dict, probe: str, claim) -> tuple[dict, str]:
    """`document` with one successful acquisition recorded, plus its row.

    IDEMPOTENT ON THE TOKEN, and only on the token. Recording an
    acquisition the census already holds is a no-op that installs the
    identical document, so a recorder that retries after an ambiguous
    failure cannot duplicate the record. The same token carrying
    DIFFERENT metadata is a controlled refusal instead: two distinct
    acquisitions sharing one token would mean the token generator
    stopped being unique, which is exactly the condition every
    ownership-safe operation downstream depends on, so it must surface
    rather than be appended past or silently overwritten.

    A claim appends to `claims` and touches nothing else — no cohort, no
    sample, no attempt, no policy field. The preservation guard enforces
    that from the other side.
    """
    if not isinstance(claim, dict):
        raise CensusError(
            f"probe {probe!r}: a claim record must be a JSON object, got "
            f"{type(claim).__name__}")
    token = claim.get("token")
    if not isinstance(token, str) or not token:
        raise CensusError(
            f"probe {probe!r}: a claim record must carry a non-empty string "
            f"`token`, got {token!r}")
    entries = _rows(document, "census")
    row = target_row(document, probe, "a claim acquisition")
    existing = find_claim(row.get("census"), token)
    record = _deep_copy(claim)
    if existing is not None:
        if existing != record:
            raise CensusError(
                f"probe {probe!r}: acquisition token {token!r} is already "
                f"recorded with different metadata, so this is not a replay "
                f"of that acquisition; an acquisition token must be unique")
        # A genuine replay: return the document unchanged, so the write
        # path installs the identical bytes and the file is not touched.
        return _deep_copy(document), probe

    rows = [dict(entry) for entry in entries]
    for candidate in rows:
        if candidate.get("key") != probe:
            continue
        census = _deep_copy(candidate["census"])
        census["claims"] = _appendable(census, "claims", probe) + [record]
        candidate["census"] = census
        break
    updated = dict(document)
    updated["probes"] = rows
    return updated, probe


def find_outcome(census, attempt: str):
    """The stored outcome carrying `attempt`, or None. A plain lookup."""
    for outcome in (census or {}).get("outcomes") or []:
        if isinstance(outcome, dict) and outcome.get("attempt") == attempt:
            return outcome
    return None


def ingest_outcome(document: dict, probe: str, outcome) -> tuple[dict, str]:
    """`document` with one stable de-flake outcome recorded, plus its row.

    IDEMPOTENT ON THE ATTEMPT IDENTITY, and only on it. Resuming an
    attempt the census already holds is a no-op that installs the
    identical document, so a workflow that resumes after an ambiguous
    failure cannot append the same outcome twice. The same identity
    carrying DIFFERENT evidence is a controlled refusal instead: two
    distinct attempts sharing one identity would mean the identity
    stopped identifying an attempt, which is the whole basis of the
    resume, so it must surface rather than be appended past or silently
    overwritten. That is deliberately `ingest_claim`'s rule with a
    different key — a de-flake attempt and a lock acquisition are
    resumed the same way.

    An outcome appends to `outcomes` and touches nothing else — no
    cohort, no sample, no attempt, no claim, no policy field. The
    preservation guard enforces that from the other side.

    The record names its own probe as well as sitting in that probe's
    row, because an outcome document is handed BETWEEN workflows; the
    two are required to agree here rather than trusted to.
    """
    if not isinstance(outcome, dict):
        raise CensusError(
            f"probe {probe!r}: an outcome record must be a JSON object, got "
            f"{type(outcome).__name__}")
    attempt = outcome.get("attempt")
    if not isinstance(attempt, str) or not attempt:
        raise CensusError(
            f"probe {probe!r}: an outcome record must carry a non-empty "
            f"string `attempt` identity, got {attempt!r}")
    if outcome.get("probe") != probe:
        raise CensusError(
            f"probe {probe!r}: outcome {attempt!r} names probe "
            f"{outcome.get('probe')!r}, so it is not this row's outcome")
    entries = _rows(document, "census")
    row = target_row(document, probe, "a diagnosis outcome")
    existing = find_outcome(row.get("census"), attempt)
    record = _deep_copy(outcome)
    if existing is not None:
        if existing != record:
            raise CensusError(
                f"probe {probe!r}: attempt {attempt!r} is already recorded "
                f"with different evidence, so this is not a resume of that "
                f"attempt; an attempt identity must be unique")
        # A genuine resume: return the document unchanged, so the write
        # path installs the identical bytes and the file is not touched.
        return _deep_copy(document), probe

    rows = [dict(entry) for entry in entries]
    for candidate in rows:
        if candidate.get("key") != probe:
            continue
        census = _deep_copy(candidate["census"])
        census["outcomes"] = _appendable(census, "outcomes", probe) + [record]
        candidate["census"] = census
        break
    updated = dict(document)
    updated["probes"] = rows
    return updated, probe


def set_policy(document: dict, probe: str, *,
               acceptable_failures=KEEP, justification=KEEP,
               estimate=KEEP) -> tuple[dict, str]:
    """Store the supplied X / justification / estimate for one probe.

    `KEEP` leaves a field alone; `None` clears it. The two are
    INDEPENDENT: clearing `acceptable_failures` does not touch its
    justification, because the maintainer's typed rationale is durable
    accumulated policy that no X update may destroy as a side effect
    (#1479). Every unrelated policy field and all measurement history
    are left exactly as they were.

    This stores the policy it is given. What makes a stored value
    ADMISSIBLE is #1430's: the declared schema bounds X, and
    `policy_invariants` refuses a candidate whose X is null, out of
    range, tolerant without a reason, or tolerant on a CI-eligible
    probe. The refusal happens in `update`, so a rejected policy leaves
    the authoritative bytes exactly as they were.
    """
    entries = _rows(document, "census")
    target_row(document, probe, "a policy update")
    rows = [dict(entry) for entry in entries]
    for row in rows:
        if row.get("key") != probe:
            continue
        census = _deep_copy(row["census"])
        if acceptable_failures is not KEEP:
            census["acceptable_failures"] = acceptable_failures
        if justification is not KEEP:
            census["acceptable_failures_justification"] = justification
        if estimate is not KEEP:
            census["estimated_worst_case_seconds"] = estimate
        row["census"] = census
        break
    updated = dict(document)
    updated["probes"] = rows
    return updated, probe


def require_deferral_text(value, what: str) -> str:
    """A non-blank deferral sentence, preserved verbatim."""
    if not isinstance(value, str) or not value.strip():
        raise CensusError(f"{what} must be a non-blank string, got {value!r}")
    if len(value) > 4000:
        raise CensusError(f"{what} is longer than 4000 characters")
    return value


def require_deferral(value, what: str):
    """A complete deferral object or null, for selector-facing reads."""
    if value is None:
        return None
    if not isinstance(value, dict):
        raise CensusError(f"{what} must be an object or null, got {value!r}")
    expected = {"reason", "resume_when"}
    if set(value) != expected:
        raise CensusError(
            f"{what} must contain exactly {sorted(expected)}, got "
            f"{sorted(value) if all(isinstance(k, str) for k in value) else list(value)!r}")
    return {
        "reason": require_deferral_text(value["reason"], f"{what}.reason"),
        "resume_when": require_deferral_text(
            value["resume_when"], f"{what}.resume_when"),
    }


def set_deferral(document: dict, probe: str, *, reason=KEEP,
                 resume_when=KEEP, resume: bool = False) -> tuple[dict, str]:
    """Defer or resume one probe without changing any retained evidence.

    Deferring requires both human-facing fields in one operation. Resuming
    clears only the current availability gate; measurements, attempts,
    claims, outcomes and policy remain byte-for-byte equal.
    """
    entries = _rows(document, "census")
    target_row(document, probe, "a deferral update")
    if resume:
        if reason is not KEEP or resume_when is not KEEP:
            raise CensusError(
                "resuming a probe cannot also supply a deferral reason or "
                "resume condition")
        deferred = None
    else:
        if reason is KEEP or resume_when is KEEP:
            raise CensusError(
                "deferring a probe requires both a reason and a resume "
                "condition")
        deferred = {
            "reason": require_deferral_text(reason, "the deferral reason"),
            "resume_when": require_deferral_text(
                resume_when, "the deferral resume condition"),
        }
    rows = [dict(entry) for entry in entries]
    for row in rows:
        if row.get("key") != probe:
            continue
        census = _deep_copy(row["census"])
        census["deferred"] = deferred
        row["census"] = census
        break
    updated = dict(document)
    updated["probes"] = rows
    return updated, probe
