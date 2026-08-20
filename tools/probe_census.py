#!/usr/bin/env python3
"""The probe census: one durable record per probe, and its atomic writer.

`docs/probe_census.json` is the de-flake lab's durable record. #1425
seeded it as an INVENTORY — every registered probe exactly once, with
its script, its CI-eligible/manual-only classification, and its protocol
status (`legacy` or `probe-result/v1`). #1428 extends that SAME file
with the measurements the lab accumulates. There is deliberately no
second file and no 75-entry variant: the inventory covers every
registered probe, and only manual-only entries carry current samples.

What a census record holds, for each probe:

* `acceptable_failures` — X, the nullable acceptable-failure count for a
  commit cohort, with an optional justification. This module STORES the
  supplied policy value; choosing it is somebody else's job.
* `estimated_worst_case_seconds` — supplied metadata, deliberately
  distinct from the OBSERVED `worst_elapsed_seconds` of a sample.
* `current` — the current commit cohort: the cohort of the most recently
  accepted measurement commit. Another measurement for the SAME commit
  appends to it; a different commit archives the whole prior cohort into
  `history` first. No history entry is ever overwritten or discarded.
* `history` — archived cohorts, append-only, retained forever. A probe
  promoted to CI eligibility keeps its history; it just stops receiving
  current samples.
* `attempts` — an append-only log of well-formed ingestion attempts. A
  well-formed harness-error result is logged but contributes no sample
  and no aggregate. Malformed input is REJECTED without touching the
  census, so it cannot be logged here either.

Only summarized outcomes and external artifact references live here.
Raw stdout, protocol event streams and engine logs stay in the
harness's artifact tree, outside every worktree.

The census lives in the worktree whose branch is `docs-wip` and is NOT
published as part of this work, so it is resolved BY BRANCH the way
`tools/docs_land.sh` does — never a hard-coded path, never the primary
checkout (which the PR drainer must be able to fast-forward), and never
created implicitly. Nothing at runtime may depend on it:
`tools/probe_flake.py` decides protocol status from
`probe_flake.PROTOCOL_PROBES` and check identity from each probe's own
descriptor, so a fresh checkout with no docs worktree behaves
identically.

Every mutation is one locked read-modify-write. The lock is a real
cross-process `flock`, keyed by the RESOLVED target path so two
processes writing the same census always contend, and it is held from
the initial read through candidate validation to the replacement.
Replacement writes a same-filesystem temporary file and `os.replace`s
it, so every observer sees either the complete old document or the
complete validated new one — never a partial write, and never a stale
temporary promoted to authoritative.

Usage:
  python3 tools/probe_census.py --print            # the manifest, to stdout
  python3 tools/probe_census.py --seed             # create/migrate in docs-wip
  python3 tools/probe_census.py --validate         # check the docs-wip copy
  python3 tools/probe_census.py --record RESULT    # ingest one measurement
  python3 tools/probe_census.py --show --probe KEY
  python3 tools/probe_census.py --probe KEY --set-acceptable-failures 2 \
      --justification "two known engine-side races"
  python3 tools/probe_census.py --probe KEY --set-estimate 480
"""
from __future__ import annotations

import argparse
import fcntl
import json
import math
import os
import re
import subprocess
import sys
import tempfile
from contextlib import contextmanager
from datetime import datetime
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ci_probes  # noqa: E402
import probe_flake  # noqa: E402
import probe_protocol  # noqa: E402
import run_probes  # noqa: E402

CENSUS_SCHEMA = "probe-census/v2"
SEED_SCHEMA = "probe-census/v1"
# `MANIFEST_SCHEMA` is the schema this module writes today. It is
# deliberately the same name #1425 used: the inventory half of the
# document did not change, only the version it is carried in.
MANIFEST_SCHEMA = CENSUS_SCHEMA
MIGRATABLE_SCHEMAS = (SEED_SCHEMA, CENSUS_SCHEMA)

MANIFEST_RELPATH = "docs/probe_census.json"
DOCS_BRANCH = "docs-wip"

CI_ELIGIBLE = "ci-eligible"
MANUAL_ONLY = "manual-only"
LEGACY = "legacy"

# The result documents this census ingests.
RESULT_SCHEMA = probe_flake.RESULT_SCHEMA
RESULT_STATUSES = ("ok", "harness-error")
RUN_OUTCOMES = (probe_flake.RUN_PASS, probe_flake.RUN_FAIL,
                probe_flake.RUN_TIMEOUT)
CHECK_OUTCOMES = (probe_protocol.PASS, probe_protocol.FAIL,
                  probe_protocol.MISSING)
# The only protocol strings an entry may carry. A measurement is
# ingestible only for the CURRENT version, so an unknown or corrupted
# string can never read as "not legacy, therefore measurable".
KNOWN_PROTOCOLS = (LEGACY, probe_protocol.PROTOCOL_VERSION)

# A full hexadecimal commit hash: sha1 today, sha256 if the repository
# ever migrates. An abbreviated hash is refused — a cohort key has to be
# unambiguous.
COMMIT_RE = re.compile(r"\A[0-9a-f]{40}(?:[0-9a-f]{24})?\Z")
TIMESTAMP_FORMAT = "%Y-%m-%dT%H:%M:%SZ"

# Locks and staging files both sit BESIDE the target. The staging file
# has to, because `os.replace` is only atomic within one filesystem. The
# lock does not have to, but a `/tmp` lock would be exposed to the
# platform's temp reaper: unlinking a HELD lock file lets the next
# writer create a fresh inode, lock that, and lose an update. A sibling
# in the census's own directory has no reaper, and inherits exactly the
# target's ownership and permissions.
LOCK_SUFFIX = ".lock"
LOCK_NOTE = (b"tools/probe_census.py holds a cross-process flock on this "
             b"file while it rewrites the census beside it. It is never "
             b"unlinked and never committed.\n")
STAGING_PREFIX = ".probe_census."
STAGING_SUFFIX = ".tmp"

# `reconcile_inventory` and `seed` may add or refresh entries; a
# measurement update may not. `TOUCH_ANY` is how the former says so.
TOUCH_ANY = object()


class CensusError(Exception):
    """Malformed census state, or input this census refuses to ingest."""


def classification(key: str) -> str:
    """The authoritative CI classification, read from `tools/ci_probes.py`."""
    return CI_ELIGIBLE if key in ci_probes.CI_ELIGIBLE else MANUAL_ONLY


# ==========================================================================
# The document
# ==========================================================================
def empty_census() -> dict:
    """A probe's census record before anything has been measured."""
    return {
        "acceptable_failures": None,
        "acceptable_failures_justification": None,
        "estimated_worst_case_seconds": None,
        "current": None,
        "history": [],
        "attempts": [],
    }


def build_manifest() -> dict:
    """The manifest the live registry currently implies, census empty.

    This is the FRESH-SEED shape. It is never written over a document
    that already exists — see `ensure_document`.
    """
    return {
        "schema": MANIFEST_SCHEMA,
        "probes": [
            {
                "key": key,
                "script": script,
                "classification": classification(key),
                "protocol": probe_flake.protocol_status(key),
                "census": empty_census(),
            }
            for key, script, _purpose in run_probes.PROBES
        ],
    }


def render_manifest(manifest: dict | None = None) -> str:
    # `is None`, not falsiness: the write path renders a CANDIDATE here,
    # and an empty one must serialize as the empty document it is rather
    # than quietly becoming a freshly generated inventory.
    document = build_manifest() if manifest is None else manifest
    return json.dumps(document, indent=2, sort_keys=True) + "\n"


def validate_manifest(manifest) -> list[str]:
    """Every disagreement between `manifest` and the live registry.

    Rejects a missing, duplicate, or extra entry, and any row whose
    classification or protocol status disagrees with `run_probes.PROBES`,
    `tools/ci_probes.py`, and `probe_flake.PROTOCOL_PROBES`. An empty
    list means the manifest is a faithful inventory. This is the
    INVENTORY check only; `validate_structure` is the one the write path
    runs, because a census must stay writable while the registry drifts.
    """
    problems: list[str] = []
    if not isinstance(manifest, dict):
        return [f"manifest must be a JSON object, got {type(manifest).__name__}"]
    schema = manifest.get("schema")
    if schema != MANIFEST_SCHEMA:
        problems.append(
            f"manifest schema is {schema!r}, expected {MANIFEST_SCHEMA!r}")
    entries = manifest.get("probes")
    if not isinstance(entries, list):
        return problems + ["manifest `probes` must be a list"]

    expected = {key: (script, classification(key), probe_flake.protocol_status(key))
                for key, script, _purpose in run_probes.PROBES}
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
                f"extra entry {key!r}: not registered in run_probes.PROBES")
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


# --------------------------------------------------------------------------
# Structural validation — what the write path checks
# --------------------------------------------------------------------------
def _is_number(value) -> bool:
    return (isinstance(value, (int, float)) and not isinstance(value, bool)
            and math.isfinite(value))


def _is_count(value) -> bool:
    return isinstance(value, int) and not isinstance(value, bool) and value >= 0


def _elapsed_tolerance(count: int) -> float:
    """How far a serialized total may sit from the serialized runs' sum.

    `probe_flake.Measurement.to_document` rounds each run's elapsed time
    to three decimals INDEPENDENTLY of `total_elapsed_seconds`, which it
    rounds from the UNROUNDED values. Each run therefore contributes up
    to 0.0005 of drift, and the total another 0.0005. `--runs` has no
    upper bound, so a fixed tolerance would reject a perfectly genuine
    long measurement: 30 runs of 1.00049 s serialize as thirty 1.0 values
    against a 30.015 total.
    """
    return 0.0005 * count + 0.0015


def _check_run_series(runs, where: str) -> tuple[list[str], list[str], list[float]]:
    """Per-run outcomes and elapsed times, plus every structural problem.

    Shared by the incoming-result validator and the stored-sample one:
    a sample that was accepted yesterday has to satisfy today the same
    rules an arriving result does, or a hand-corrupted census would be
    readable and rewritable.
    """
    problems: list[str] = []
    outcomes: list[str] = []
    elapsed: list[float] = []
    indices: list[int] = []
    for position, record in enumerate(runs):
        if not isinstance(record, dict):
            problems.append(f"{where} run {position} is not an object")
            continue
        index = record.get("index")
        if not _is_count(index):
            problems.append(f"{where} run {position} has no integer `index`")
        else:
            indices.append(index)
        outcome = record.get("outcome")
        if outcome not in RUN_OUTCOMES:
            problems.append(f"{where} run {position} outcome {outcome!r} is "
                            f"not one of {RUN_OUTCOMES}")
        else:
            outcomes.append(outcome)
        value = record.get("elapsed_seconds")
        if not _is_number(value) or value < 0:
            problems.append(f"{where} run {position} `elapsed_seconds` must be "
                            f"a finite non-negative number")
        else:
            elapsed.append(float(value))
        artifact = record.get("artifact_dir")
        if artifact is not None and not isinstance(artifact, str):
            problems.append(f"{where} run {position} `artifact_dir` must be a "
                            f"string or null")
    if indices and len(indices) == len(runs) \
            and indices != list(range(1, len(indices) + 1)):
        # `probe_flake.measure` numbers runs from 1 and stops at the
        # first untrustworthy stream, so the valid runs are always a
        # contiguous 1..completed prefix.
        problems.append(f"{where} run indices {indices[:6]} are not the "
                        f"contiguous 1..{len(indices)} sequence")
    return problems, outcomes, elapsed


def _check_aggregates(record, where: str, outcomes: list[str],
                      elapsed: list[float], complete: bool) -> list[str]:
    """The failure/timeout/rate/duration aggregates against the runs."""
    problems: list[str] = []
    failures = sum(1 for o in outcomes
                   if o in (probe_flake.RUN_FAIL, probe_flake.RUN_TIMEOUT))
    timeouts = sum(1 for o in outcomes if o == probe_flake.RUN_TIMEOUT)
    if record.get("failure_count") != failures:
        problems.append(f"{where} reports failure_count="
                        f"{record.get('failure_count')!r} but the runs show "
                        f"{failures}")
    if record.get("timeout_count") != timeouts:
        problems.append(f"{where} reports timeout_count="
                        f"{record.get('timeout_count')!r} but the runs show "
                        f"{timeouts}")
    tolerance = _elapsed_tolerance(len(elapsed))
    for field, expected, slack in (
            ("worst_elapsed_seconds", max(elapsed, default=0.0), 0.0015),
            ("total_elapsed_seconds", sum(elapsed), tolerance)):
        value = record.get(field)
        if not _is_number(value) or value < 0:
            problems.append(f"{where} `{field}` must be a finite non-negative "
                            f"number")
        elif complete and abs(value - expected) > slack:
            problems.append(f"{where} reports {field}={value} but the runs "
                            f"give {round(expected, 3)}")
    return problems


def _validate_sample(sample, where: str) -> list[str]:
    """A STORED sample, held to the rules its result document met.

    Nothing here is weaker than `validate_result`: a sample whose
    timestamp, commit hash, run series or aggregates were corrupted
    after the fact must fail to READ, so no later write can carry it
    forward or rewrite the file around it.
    """
    if not isinstance(sample, dict):
        return [f"{where} is not an object: {sample!r}"]
    problems: list[str] = []
    commit = sample.get("commit_sha")
    if not isinstance(commit, str) or not COMMIT_RE.match(commit):
        problems.append(f"{where} `commit_sha` is not a full commit hash: "
                        f"{commit!r}")
    stamp = sample.get("timestamp_utc")
    if not isinstance(stamp, str):
        problems.append(f"{where} has no string `timestamp_utc`")
    else:
        try:
            datetime.strptime(stamp, TIMESTAMP_FORMAT)
        except ValueError:
            problems.append(f"{where} `timestamp_utc` {stamp!r} is not "
                            f"{TIMESTAMP_FORMAT}")
    for field in ("requested_runs", "completed_runs", "failure_count",
                  "timeout_count"):
        if not _is_count(sample.get(field)):
            problems.append(f"{where} `{field}` must be a non-negative integer")
    requested = sample.get("requested_runs")
    completed = sample.get("completed_runs")
    if _is_count(requested) and requested < 1:
        problems.append(f"{where} `requested_runs` must be positive")
    if _is_count(requested) and _is_count(completed) and completed != requested:
        # A sample exists only because an `ok` result was accepted, and
        # an `ok` result completes every run it requested. A stored
        # partial is corruption, not a shorter measurement.
        problems.append(f"{where} completed {completed} of {requested} "
                        f"requested runs, but a stored sample always "
                        f"completes all of them")
    for field in ("rts_capabilities", "peak_concurrency"):
        value = sample.get(field)
        if not _is_count(value) or value < 1:
            problems.append(f"{where} `{field}` must be a positive integer")
    runs = sample.get("runs")
    if not isinstance(runs, list):
        problems.append(f"{where} `runs` must be a list")
        runs = []
    elif _is_count(completed) and len(runs) != completed:
        problems.append(f"{where} lists {len(runs)} runs but reports "
                        f"completed_runs={completed}")
    series, outcomes, elapsed = _check_run_series(runs, where)
    problems += series
    problems += _check_aggregates(sample, where, outcomes, elapsed,
                                  complete=len(elapsed) == len(runs))
    rate = sample.get("failure_rate")
    if not (_is_number(rate) and 0.0 <= rate <= 1.0):
        problems.append(f"{where} `failure_rate` must be a number in [0, 1]")
    elif _is_count(requested) and requested and len(outcomes) == len(runs):
        failures = sum(1 for o in outcomes
                       if o in (probe_flake.RUN_FAIL, probe_flake.RUN_TIMEOUT))
        if abs(rate - failures / requested) > 1e-6:
            problems.append(f"{where} reports failure_rate={rate} but "
                            f"{failures}/{requested} is "
                            f"{round(failures / requested, 6)}")
    counts = sample.get("check_counts")
    if not isinstance(counts, dict) or not counts:
        problems.append(f"{where} `check_counts` must be a non-empty object")
    else:
        for cid, tally in counts.items():
            if not isinstance(tally, dict):
                problems.append(f"{where} check {cid!r} tally is not an object")
                continue
            total = 0
            usable = True
            for outcome in CHECK_OUTCOMES:
                if not _is_count(tally.get(outcome)):
                    problems.append(f"{where} check {cid!r} has no "
                                    f"non-negative `{outcome}` count")
                    usable = False
                else:
                    total += tally[outcome]
            # A stored sample keeps no per-run check results, so this is
            # the surviving cross-check: every declared check was
            # resolved exactly once per completed run.
            if usable and _is_count(completed) and total != completed:
                problems.append(f"{where} check {cid!r} tallies {total} "
                                f"outcomes across {completed} completed runs")
    artifacts = sample.get("retained_artifacts")
    if not isinstance(artifacts, list) or any(
            not isinstance(a, str) for a in artifacts):
        problems.append(f"{where} `retained_artifacts` must be a list of strings")
    return problems


def _validate_cohort(cohort, where: str) -> list[str]:
    if not isinstance(cohort, dict):
        return [f"{where} is not an object: {cohort!r}"]
    problems: list[str] = []
    commit = cohort.get("commit_sha")
    if not isinstance(commit, str) or not COMMIT_RE.match(commit):
        problems.append(f"{where} `commit_sha` is not a full commit hash: "
                        f"{commit!r}")
    samples = cohort.get("samples")
    if not isinstance(samples, list) or not samples:
        problems.append(f"{where} `samples` must be a non-empty list")
        return problems
    for position, sample in enumerate(samples):
        problems += _validate_sample(sample, f"{where} sample {position}")
        if isinstance(sample, dict) and sample.get("commit_sha") != commit:
            problems.append(
                f"{where} sample {position} commit {sample.get('commit_sha')!r} "
                f"does not belong to the cohort ({commit!r})")
    return problems


def _validate_attempt(attempt, where: str) -> list[str]:
    if not isinstance(attempt, dict):
        return [f"{where} is not an object: {attempt!r}"]
    problems: list[str] = []
    if not isinstance(attempt.get("status"), str) or not attempt["status"]:
        problems.append(f"{where} has no string `status`")
    commit = attempt.get("commit_sha")
    if not isinstance(commit, str) or not COMMIT_RE.match(commit):
        problems.append(f"{where} `commit_sha` is not a full commit hash: "
                        f"{commit!r}")
    stamp = attempt.get("timestamp_utc")
    if not isinstance(stamp, str):
        problems.append(f"{where} has no string `timestamp_utc`")
    else:
        try:
            datetime.strptime(stamp, TIMESTAMP_FORMAT)
        except ValueError:
            problems.append(f"{where} `timestamp_utc` {stamp!r} is not "
                            f"{TIMESTAMP_FORMAT}")
    if attempt.get("status") not in RESULT_STATUSES and isinstance(
            attempt.get("status"), str):
        problems.append(f"{where} status {attempt['status']!r} is not one of "
                        f"{RESULT_STATUSES}")
    if not isinstance(attempt.get("accepted"), bool):
        problems.append(f"{where} `accepted` must be a boolean")
    for field in ("requested_runs", "completed_runs"):
        if not _is_count(attempt.get(field)):
            problems.append(f"{where} `{field}` must be a non-negative integer")
    requested, completed = attempt.get("requested_runs"), attempt.get("completed_runs")
    if _is_count(requested) and _is_count(completed) and completed > requested:
        problems.append(f"{where} completed {completed} of {requested} "
                        f"requested runs")
    error = attempt.get("error")
    if error is not None and not isinstance(error, str):
        problems.append(f"{where} `error` must be a string or null")
    # `accepted` is DERIVED from the status at ingestion, so the two can
    # only disagree in a record somebody edited.
    status = attempt.get("status")
    accepted = attempt.get("accepted")
    if isinstance(accepted, bool) and status in RESULT_STATUSES:
        if accepted != (status == "ok"):
            problems.append(f"{where} is {status!r} but accepted={accepted}")
    if status == "ok":
        if error is not None:
            problems.append(f"{where} is ok but carries an `error`")
        if _is_count(requested) and _is_count(completed) and completed != requested:
            problems.append(f"{where} is ok but completed {completed} of "
                            f"{requested} requested runs")
    elif status == "harness-error":
        if not isinstance(error, str) or not error:
            problems.append(f"{where} is a harness error with no `error`")
        if _is_count(requested) and _is_count(completed) and completed >= requested:
            # The run that broke the stream is never one of the completed
            # ones, so a harness error always stops short.
            problems.append(f"{where} is a harness error but completed all "
                            f"{requested} requested runs")
    artifacts = attempt.get("retained_artifacts")
    if not isinstance(artifacts, list) or any(
            not isinstance(a, str) for a in artifacts):
        problems.append(f"{where} `retained_artifacts` must be a list of strings")
    return problems


def _validate_census(census, where: str) -> list[str]:
    if not isinstance(census, dict):
        return [f"{where} `census` is not an object: {census!r}"]
    problems: list[str] = []
    x = census.get("acceptable_failures")
    if x is not None and not _is_count(x):
        problems.append(
            f"{where} `acceptable_failures` must be null or a non-negative "
            f"integer, got {x!r}")
    justification = census.get("acceptable_failures_justification")
    if justification is not None and (not isinstance(justification, str)
                                      or not justification.strip()):
        problems.append(
            f"{where} `acceptable_failures_justification` must be null or a "
            f"non-empty string")
    estimate = census.get("estimated_worst_case_seconds")
    if estimate is not None and (not _is_number(estimate) or estimate < 0):
        problems.append(
            f"{where} `estimated_worst_case_seconds` must be null or a finite "
            f"non-negative number, got {estimate!r}")
    current = census.get("current")
    if current is not None:
        problems += _validate_cohort(current, f"{where} current cohort")
    history = census.get("history")
    if not isinstance(history, list):
        problems.append(f"{where} `history` must be a list")
    else:
        for position, cohort in enumerate(history):
            problems += _validate_cohort(cohort, f"{where} history[{position}]")
    attempts = census.get("attempts")
    if not isinstance(attempts, list):
        problems.append(f"{where} `attempts` must be a list")
    else:
        for position, attempt in enumerate(attempts):
            problems += _validate_attempt(attempt, f"{where} attempts[{position}]")
    return problems


def validate_structure(document, *, include_promotion: bool = True) -> list[str]:
    """Everything the write path checks before installing a candidate.

    Schema, the inventory row of EVERY entry, and every census record.
    Deliberately independent of the live registry: the census must stay
    writable while `run_probes.PROBES` drifts ahead of it — that drift is
    `validate_manifest`'s report and `--seed`'s repair, not a reason to
    refuse a measurement.

    `include_promotion` is the one rule a READER relaxes: a stored
    CI-eligible entry still carrying a manual-only current cohort is a
    candidate this tool refuses to write, but reading it has to succeed
    or `reconcile_inventory` — the designated repair — could never run.
    """
    if not isinstance(document, dict):
        return [f"census must be a JSON object, got {type(document).__name__}"]
    problems: list[str] = []
    if document.get("schema") != CENSUS_SCHEMA:
        problems.append(f"census schema is {document.get('schema')!r}, "
                        f"expected {CENSUS_SCHEMA!r}")
    entries = document.get("probes")
    if not isinstance(entries, list):
        return problems + ["census `probes` must be a list"]
    seen: set[str] = set()
    for position, entry in enumerate(entries):
        if not isinstance(entry, dict):
            problems.append(f"entry {position} is not an object: {entry!r}")
            continue
        key = entry.get("key")
        if not isinstance(key, str) or not key:
            problems.append(f"entry {position} has no string `key`")
            continue
        if key in seen:
            problems.append(f"duplicate entry for probe {key!r}")
            continue
        seen.add(key)
        if not isinstance(entry.get("script"), str) or not entry["script"]:
            problems.append(f"probe {key!r} has no string `script`")
        if entry.get("protocol") not in KNOWN_PROTOCOLS:
            problems.append(
                f"probe {key!r} protocol {entry.get('protocol')!r} is not one "
                f"of {KNOWN_PROTOCOLS}")
        if entry.get("classification") not in (CI_ELIGIBLE, MANUAL_ONLY):
            problems.append(
                f"probe {key!r} classification {entry.get('classification')!r} "
                f"is not one of {(CI_ELIGIBLE, MANUAL_ONLY)}")
        problems += _validate_census(entry.get("census"), f"probe {key!r}")
        if (include_promotion
                and entry.get("classification") == CI_ELIGIBLE
                and isinstance(entry.get("census"), dict)
                and entry["census"].get("current") is not None):
            problems.append(
                f"probe {key!r} is CI-eligible but still carries a current "
                f"manual-only cohort")
    return problems


# --------------------------------------------------------------------------
# Migration and inventory reconciliation
# --------------------------------------------------------------------------
def migrate_document(document) -> dict:
    """A `probe-census/v1` or `/v2` document, as `/v2`.

    The one-time migration adds an empty census record to every entry
    that lacks one. It preserves the entry list exactly — same entries,
    same order, same inventory columns — because the seeded document is
    migration INPUT, never something to regenerate.
    """
    if not isinstance(document, dict):
        raise CensusError(
            f"census must be a JSON object, got {type(document).__name__}")
    schema = document.get("schema")
    if schema not in MIGRATABLE_SCHEMAS:
        raise CensusError(
            f"census schema {schema!r} is not one this tool can read "
            f"(expected one of {MIGRATABLE_SCHEMAS})")
    entries = document.get("probes")
    if not isinstance(entries, list):
        raise CensusError("census `probes` must be a list")
    migrated = []
    for position, entry in enumerate(entries):
        if not isinstance(entry, dict):
            raise CensusError(f"census entry {position} is not an object")
        row = dict(entry)
        if "census" not in row:
            row["census"] = empty_census()
        migrated.append(row)
    result = dict(document)
    result["schema"] = CENSUS_SCHEMA
    result["probes"] = migrated
    problems = validate_structure(result, include_promotion=False)
    if problems:
        raise CensusError("migrated census is malformed: " +
                          "; ".join(problems[:5]))
    return result


def _archive_current(census: dict) -> None:
    """Move the current cohort into history. Never discards one."""
    if census.get("current") is not None:
        census["history"] = list(census.get("history") or []) + [census["current"]]
        census["current"] = None


def reconcile_inventory(document: dict) -> dict:
    """Refresh the inventory columns without touching accumulated data.

    Appends an entry for a probe registered since the document was
    written, refreshes `script`/`classification`/`protocol` from the live
    registry, and applies PROMOTION: an entry the live `ci_probes.py`
    now classifies CI-eligible archives its current manual-only cohort
    and stops carrying one, keeping its history and attempts. It never
    deletes an entry and never rewrites a census record — a probe that
    left `run_probes.PROBES` keeps its measurements and is reported by
    `validate_manifest` as an extra entry, which is a decision for a
    person.
    """
    result = migrate_document(document)
    live = {key: script for key, script, _purpose in run_probes.PROBES}
    entries = [dict(entry) for entry in result["probes"]]
    present = {entry["key"] for entry in entries}
    for entry in entries:
        key = entry["key"]
        if key not in live:
            continue
        entry["script"] = live[key]
        entry["protocol"] = probe_flake.protocol_status(key)
        entry["classification"] = classification(key)
        if entry["classification"] == CI_ELIGIBLE:
            census = dict(entry["census"])
            _archive_current(census)
            entry["census"] = census
    for key, script, _purpose in run_probes.PROBES:
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
def find_entry(document: dict, probe: str) -> dict | None:
    for entry in document.get("probes") or []:
        if isinstance(entry, dict) and entry.get("key") == probe:
            return entry
    return None


def validate_result(result, document: dict) -> list[str]:
    """Every reason `result` may not be ingested into `document`.

    A `probe-flake-result/v1` document is checked whole before anything
    is mutated: its probe must name an existing manual-only,
    protocol-compatible entry, its commit must be a full hexadecimal
    hash, its timestamp must parse as UTC, and its durations, run
    indices, outcomes, per-check tallies and aggregates must all be
    internally consistent. Any problem here leaves the census
    byte-for-byte unchanged.
    """
    problems: list[str] = []
    if not isinstance(result, dict):
        return [f"result must be a JSON object, got {type(result).__name__}"]
    if result.get("schema") != RESULT_SCHEMA:
        problems.append(f"result schema is {result.get('schema')!r}, expected "
                        f"{RESULT_SCHEMA!r}")
    probe = result.get("probe")
    if not isinstance(probe, str) or not probe:
        problems.append("result has no string `probe`")
        return problems
    entry = find_entry(document, probe)
    if entry is None:
        problems.append(f"probe {probe!r} has no census entry")
    else:
        if entry.get("classification") != MANUAL_ONLY:
            problems.append(
                f"probe {probe!r} is {entry.get('classification')!r} in the "
                f"census, so it takes no current census samples")
        if entry.get("protocol") != probe_protocol.PROTOCOL_VERSION:
            problems.append(
                f"probe {probe!r} is {entry.get('protocol')!r} in the census, "
                f"not {probe_protocol.PROTOCOL_VERSION!r}, so it produces no "
                f"trustworthy measurement")
    # The LIVE classification is authoritative and wins over a stale
    # census row: `tools/ci_probes.py` owns the CI/manual XOR.
    if probe in ci_probes.CI_ELIGIBLE:
        problems.append(
            f"probe {probe!r} is CI-eligible per tools/ci_probes.py, so it "
            f"takes no manual-only census samples")

    status = result.get("status")
    if status not in RESULT_STATUSES:
        problems.append(f"result status {status!r} is not one of "
                        f"{RESULT_STATUSES}")
    commit = result.get("commit_sha")
    if not isinstance(commit, str) or not COMMIT_RE.match(commit):
        problems.append(f"result `commit_sha` is not a full commit hash: "
                        f"{commit!r}")
    stamp = result.get("timestamp_utc")
    if not isinstance(stamp, str):
        problems.append("result has no string `timestamp_utc`")
    else:
        try:
            datetime.strptime(stamp, TIMESTAMP_FORMAT)
        except ValueError:
            problems.append(
                f"result `timestamp_utc` {stamp!r} is not {TIMESTAMP_FORMAT}")

    requested = result.get("requested_runs")
    completed = result.get("completed_runs")
    if not _is_count(requested) or requested < 1:
        problems.append("result `requested_runs` must be a positive integer")
    if not _is_count(completed):
        problems.append("result `completed_runs` must be a non-negative integer")
    if _is_count(requested) and _is_count(completed) and completed > requested:
        problems.append(
            f"result completed {completed} of {requested} requested runs")

    for field in ("rts_capabilities", "peak_concurrency"):
        value = result.get(field)
        if not _is_count(value) or value < 1:
            problems.append(f"result `{field}` must be a positive integer")

    runs = result.get("runs")
    if not isinstance(runs, list):
        problems.append("result `runs` must be a list")
        runs = []
    elif _is_count(completed) and len(runs) != completed:
        problems.append(f"result lists {len(runs)} runs but reports "
                        f"completed_runs={completed}")
    declared: set = set()
    declared_checks = result.get("checks")
    if not isinstance(declared_checks, list) or not declared_checks:
        problems.append("result `checks` must be a non-empty list of "
                        "{id, label} objects")
    else:
        for position, check in enumerate(declared_checks):
            if not isinstance(check, dict) or not isinstance(check.get("id"), str) \
                    or not check["id"] or not isinstance(check.get("label"), str):
                problems.append(f"declared check {position} is not an "
                                f"{{id, label}} object: {check!r}")
                continue
            if check["id"] in declared:
                problems.append(f"declared check {check['id']!r} appears twice")
            declared.add(check["id"])
    # The run series, its indices and the aggregates over it are checked
    # by the SAME helpers a stored sample goes through, so a document
    # this validator accepts cannot become a sample the reader rejects.
    series, outcomes, elapsed_values = _check_run_series(runs, "result")
    problems += series
    for position, record in enumerate(runs):
        if not isinstance(record, dict):
            continue
        checks = record.get("checks")
        if not isinstance(checks, dict):
            problems.append(f"run {position} `checks` must be an object")
            continue
        if declared and set(checks) != declared:
            problems.append(
                f"run {position} reports checks {sorted(checks)} but the "
                f"descriptor declares {sorted(declared)}")
        for cid, value in checks.items():
            if value not in CHECK_OUTCOMES:
                problems.append(
                    f"run {position} check {cid!r} result {value!r} is not "
                    f"one of {CHECK_OUTCOMES}")

    counts = result.get("check_counts")
    if not isinstance(counts, dict):
        problems.append("result `check_counts` must be an object")
    else:
        if declared and set(counts) != declared:
            problems.append(
                f"result `check_counts` covers {sorted(counts)} but the "
                f"descriptor declares {sorted(declared)}")
        observed = {cid: {o: 0 for o in CHECK_OUTCOMES} for cid in counts}
        for record in runs:
            if not isinstance(record, dict):
                continue
            for cid, value in (record.get("checks") or {}).items():
                if cid in observed and value in CHECK_OUTCOMES:
                    observed[cid][value] += 1
        for cid, tally in counts.items():
            if not isinstance(tally, dict):
                problems.append(f"check {cid!r} tally is not an object")
                continue
            for outcome in CHECK_OUTCOMES:
                if not _is_count(tally.get(outcome)):
                    problems.append(f"check {cid!r} has no non-negative "
                                    f"`{outcome}` count")
                elif tally[outcome] != observed[cid][outcome]:
                    problems.append(
                        f"check {cid!r} reports {tally[outcome]} {outcome} but "
                        f"the runs show {observed[cid][outcome]}")

    problems += _check_aggregates(result, "result", outcomes, elapsed_values,
                                  complete=len(elapsed_values) == len(runs))
    failures = sum(1 for o in outcomes
                   if o in (probe_flake.RUN_FAIL, probe_flake.RUN_TIMEOUT))

    rate = result.get("failure_rate")
    if status == "ok":
        if result.get("error") is not None:
            problems.append("an ok result must carry no `error`")
        if result.get("error_run") is not None:
            problems.append("an ok result must carry no `error_run`")
        if _is_count(requested) and _is_count(completed) and completed != requested:
            problems.append(f"an ok result must complete all {requested} runs, "
                            f"got {completed}")
        if not _is_number(rate) or not 0.0 <= rate <= 1.0:
            problems.append("an ok result must carry a `failure_rate` in [0, 1]")
        elif _is_count(requested) and requested and abs(
                rate - failures / requested) > 1e-6:
            problems.append(
                f"result reports failure_rate={rate} but {failures}/{requested} "
                f"is {round(failures / requested, 6)}")
    elif status == "harness-error":
        if not isinstance(result.get("error"), str) or not result["error"]:
            problems.append("a harness-error result must carry a non-empty "
                            "`error`")
        if rate is not None:
            problems.append("a harness-error result must carry no "
                            "`failure_rate` — no trustworthy rate exists")
        if _is_count(requested) and _is_count(completed) and completed >= requested:
            problems.append(f"a harness-error result stops short, but this "
                            f"one completed all {requested} requested runs")
        problems += _validate_error_run(result.get("error_run"), declared,
                                        completed, requested)
    artifacts = result.get("retained_artifacts")
    if not isinstance(artifacts, list) or any(
            not isinstance(a, str) for a in artifacts):
        problems.append("result `retained_artifacts` must be a list of strings")
    return problems


def _validate_error_run(record, declared: set, completed, requested) -> list[str]:
    """The one run whose protocol stream could not be trusted.

    `probe_flake.measure` ALWAYS records this run on a harness error —
    it is what stops the result reading as "nothing went wrong, nothing
    retained" — so its absence is malformed input, not a lighter kind of
    harness error. It is deliberately not one of the completed runs, so
    it sits at exactly the next index.
    """
    if record is None:
        return ["a harness-error result must carry its `error_run`"]
    if not isinstance(record, dict):
        return [f"`error_run` is not an object: {record!r}"]
    problems: list[str] = []
    if record.get("outcome") != probe_flake.RUN_HARNESS_ERROR:
        problems.append(f"`error_run` outcome {record.get('outcome')!r} is not "
                        f"{probe_flake.RUN_HARNESS_ERROR!r}")
    index = record.get("index")
    if not _is_count(index):
        problems.append("`error_run` has no integer `index`")
    elif _is_count(completed) and index != completed + 1:
        problems.append(f"`error_run` index {index} is not the run after the "
                        f"{completed} completed ones")
    elif _is_count(requested) and index > requested:
        problems.append(f"`error_run` index {index} exceeds the {requested} "
                        f"requested runs")
    value = record.get("elapsed_seconds")
    if not _is_number(value) or value < 0:
        problems.append("`error_run` `elapsed_seconds` must be a finite "
                        "non-negative number")
    artifact = record.get("artifact_dir")
    if artifact is not None and not isinstance(artifact, str):
        problems.append("`error_run` `artifact_dir` must be a string or null")
    checks = record.get("checks")
    if not isinstance(checks, dict):
        problems.append("`error_run` `checks` must be an object")
    else:
        if declared and set(checks) != declared:
            problems.append(f"`error_run` reports checks {sorted(checks)} but "
                            f"the descriptor declares {sorted(declared)}")
        for cid, outcome in checks.items():
            if outcome not in CHECK_OUTCOMES:
                problems.append(f"`error_run` check {cid!r} result {outcome!r} "
                                f"is not one of {CHECK_OUTCOMES}")
    return problems


def summarize_sample(result: dict) -> dict:
    """The durable record of one accepted measurement.

    Summarized outcomes and artifact REFERENCES only: no stdout, no
    protocol stream, no engine log ever enters the census.
    """
    return {
        "timestamp_utc": result["timestamp_utc"],
        "commit_sha": result["commit_sha"],
        "requested_runs": result["requested_runs"],
        "completed_runs": result["completed_runs"],
        "runs": [{"index": r["index"], "outcome": r["outcome"],
                  "elapsed_seconds": r["elapsed_seconds"],
                  "artifact_dir": r.get("artifact_dir")}
                 for r in result["runs"]],
        "check_counts": json.loads(json.dumps(result["check_counts"])),
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
    return {
        "timestamp_utc": result["timestamp_utc"],
        "commit_sha": result["commit_sha"],
        "status": result["status"],
        "accepted": accepted,
        "requested_runs": result["requested_runs"],
        "completed_runs": result["completed_runs"],
        "error": result.get("error"),
        "retained_artifacts": list(result["retained_artifacts"]),
    }


def ingest_result(document: dict, result) -> tuple[dict, str]:
    """`document` with `result` recorded, plus the probe it touched.

    Raises `CensusError` — leaving `document` untouched — for anything
    `validate_result` rejects. A well-formed harness-error result is
    logged as a failed attempt and contributes no sample and no
    aggregate. An accepted measurement appends to the current cohort
    when it names the same commit, and otherwise archives the whole
    prior cohort before opening a new one.
    """
    problems = validate_result(result, document)
    if problems:
        raise CensusError(f"refusing {RESULT_SCHEMA} document: " +
                          "; ".join(problems[:5]))
    probe = result["probe"]
    entries = [dict(entry) for entry in document["probes"]]
    for entry in entries:
        if entry["key"] != probe:
            continue
        census = json.loads(json.dumps(entry["census"]))
        accepted = result["status"] == "ok"
        if accepted:
            current = census.get("current")
            if current is None or current.get("commit_sha") != result["commit_sha"]:
                _archive_current(census)
                census["current"] = {"commit_sha": result["commit_sha"],
                                     "samples": []}
            census["current"]["samples"].append(summarize_sample(result))
        census["attempts"] = list(census.get("attempts") or []) + [
            summarize_attempt(result, accepted)]
        entry["census"] = census
        break
    updated = dict(document)
    updated["probes"] = entries
    return updated, probe


def set_policy(document: dict, probe: str, *,
               acceptable_failures="keep", justification="keep",
               estimate="keep") -> tuple[dict, str]:
    """Store the supplied X / justification / estimate for one probe.

    `"keep"` leaves a field alone; `None` clears it. This module stores
    the policy it is given and never chooses one.
    """
    entry = find_entry(document, probe)
    if entry is None:
        raise CensusError(f"probe {probe!r} has no census entry")
    entries = [dict(row) for row in document["probes"]]
    for row in entries:
        if row["key"] != probe:
            continue
        census = json.loads(json.dumps(row["census"]))
        if acceptable_failures != "keep":
            census["acceptable_failures"] = acceptable_failures
        if justification != "keep":
            census["acceptable_failures_justification"] = justification
        if estimate != "keep":
            census["estimated_worst_case_seconds"] = estimate
        row["census"] = census
        break
    updated = dict(document)
    updated["probes"] = entries
    return updated, probe


# ==========================================================================
# The docs worktree
# ==========================================================================
class DocsWorktreeMissing(Exception):
    """No worktree is on `docs-wip`; the caller must create one."""


def resolve_docs_worktree(repo_root: str | None = None) -> Path:
    """The worktree whose branch is `docs-wip`, resolved BY BRANCH.

    The same idiom `tools/docs_land.sh` uses. A missing docs worktree is
    an actionable stop, never a silent fall back to the primary checkout
    (which the PR drainer must be able to fast-forward) and never an
    implicit `git worktree add` performed as a side effect.
    """
    root = repo_root or run_probes.REPO_ROOT
    try:
        done = subprocess.run(["git", "worktree", "list", "--porcelain"],
                              cwd=root, text=True, capture_output=True,
                              timeout=30)
    except (OSError, subprocess.SubprocessError) as error:
        raise DocsWorktreeMissing(
            f"could not list git worktrees ({error})") from None
    if done.returncode != 0:
        raise DocsWorktreeMissing(
            f"could not list git worktrees: {done.stderr.strip()}")
    current: str | None = None
    for line in done.stdout.splitlines():
        if line.startswith("worktree "):
            current = line[len("worktree "):]
        elif line.strip() == f"branch refs/heads/{DOCS_BRANCH}" and current:
            return Path(current)
    raise DocsWorktreeMissing(
        f"no worktree is on branch {DOCS_BRANCH}. Create one with:\n"
        f"  git worktree add ~/work/synarchy-docs -b {DOCS_BRANCH} origin/master")


def manifest_path(repo_root: str | None = None) -> Path:
    return resolve_docs_worktree(repo_root) / MANIFEST_RELPATH


def load(path: Path):
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except OSError as error:
        raise DocsWorktreeMissing(
            f"manifest {path} is unreadable ({error})") from None
    except ValueError as error:
        raise ValueError(f"manifest {path} is not valid JSON: {error}") from None


# ==========================================================================
# The atomic write path
# ==========================================================================
def lock_path(target: Path) -> Path:
    """The one lock file guarding `target`, keyed by its RESOLVED path.

    ONE stable identity per target, so two processes naming the same
    census by different paths always contend and two different censuses
    never do.
    """
    resolved = Path(target).resolve()
    return resolved.parent / f".{resolved.name}{LOCK_SUFFIX}"


@contextmanager
def _locked(target: Path):
    _refuse_symlink(Path(target))
    guard = lock_path(target)
    try:
        # The census directory is created here rather than at
        # replacement time so the lock exists for the very first writer
        # too. Reaching this point already means a `docs-wip` worktree
        # resolved, so nothing is created anywhere else.
        guard.parent.mkdir(parents=True, exist_ok=True)
        fd = os.open(str(guard), os.O_CREAT | os.O_RDWR, 0o600)
    except OSError as error:
        raise CensusError(
            f"could not open the census lock {guard} ({error})") from None
    try:
        fcntl.flock(fd, fcntl.LOCK_EX)
        # The lock file is deliberately never unlinked: removing a HELD
        # flock file lets the next writer create a fresh inode, lock
        # that, and lose an update. It is a small untracked file in the
        # docs worktree — which is where CLAUDE.md's working-tree
        # discipline says an uncommitted file belongs — so it says so
        # itself for whoever finds it in `git status`.
        if os.fstat(fd).st_size == 0:
            os.write(fd, LOCK_NOTE)
        yield
    finally:
        try:
            fcntl.flock(fd, fcntl.LOCK_UN)
        finally:
            os.close(fd)


def _clear_staging(directory: Path) -> None:
    """Remove staging files a killed writer left behind.

    Called under the lock, so nothing live is ever removed. A stale
    staging file is never authoritative — only `os.replace` makes a
    candidate the census — but it should not accumulate either.
    """
    try:
        candidates = list(directory.iterdir())
    except OSError:
        return
    for entry in candidates:
        name = entry.name
        if name.startswith(STAGING_PREFIX) and name.endswith(STAGING_SUFFIX):
            try:
                entry.unlink()
            except OSError:
                pass


def _atomic_replace(target: Path, payload: bytes) -> None:
    """Install `payload` as `target` in one step.

    The staging file is a SIBLING so the rename never crosses a
    filesystem, the bytes are fsynced before the rename so a crash
    cannot promote a short file, and the directory is fsynced after so
    the rename itself is durable.
    """
    target.parent.mkdir(parents=True, exist_ok=True)
    fd, staged = tempfile.mkstemp(dir=str(target.parent),
                                  prefix=STAGING_PREFIX, suffix=STAGING_SUFFIX)
    staged_path = Path(staged)
    try:
        with os.fdopen(fd, "wb") as handle:
            handle.write(payload)
            handle.flush()
            os.fsync(handle.fileno())
        os.chmod(staged_path, 0o644)
        os.replace(str(staged_path), str(target))
    except BaseException:
        try:
            staged_path.unlink()
        except OSError:
            pass
        raise
    dir_fd = os.open(str(target.parent), os.O_RDONLY)
    try:
        os.fsync(dir_fd)
    finally:
        os.close(dir_fd)


def _entry_map(document) -> dict:
    return {entry["key"]: entry for entry in document.get("probes") or []
            if isinstance(entry, dict) and isinstance(entry.get("key"), str)}


def _sample_total(census) -> int:
    """Every retained sample a census record holds, current and archived."""
    if not isinstance(census, dict):
        return 0
    cohorts = list(census.get("history") or [])
    if census.get("current") is not None:
        cohorts.append(census["current"])
    return sum(len(c.get("samples") or []) for c in cohorts
               if isinstance(c, dict))


def _check_preserved(before, after, touched) -> list[str]:
    """Every way a candidate disturbed what it had no business touching.

    JSON serialization necessarily rewrites the whole file, so this is
    what makes "changes only the affected probe's record" real: every
    unrelated entry must be deeply equal and in the same position, and
    the touched entry's history and attempts must have grown by
    appending — never by rewriting or discarding.
    """
    if before is None:
        return []
    problems: list[str] = []
    before_keys = [e.get("key") for e in before.get("probes") or []]
    after_keys = [e.get("key") for e in after.get("probes") or []]
    if touched is TOUCH_ANY:
        if after_keys[:len(before_keys)] != before_keys:
            problems.append("the candidate reordered or dropped existing "
                            "inventory entries")
        return problems
    if after_keys != before_keys:
        problems.append(
            f"the candidate changed the inventory order or membership "
            f"({len(before_keys)} entries before, {len(after_keys)} after)")
        return problems
    names = set(touched)
    old = _entry_map(before)
    new = _entry_map(after)
    for key, entry in old.items():
        if key not in names:
            if new.get(key) != entry:
                problems.append(
                    f"the candidate modified unrelated probe {key!r}")
            continue
        was = (entry.get("census") or {})
        now = (new.get(key, {}).get("census") or {})
        for field in ("history", "attempts"):
            previous = was.get(field) or []
            current = now.get(field) or []
            if current[:len(previous)] != previous:
                problems.append(
                    f"probe {key!r} `{field}` is append-only, but the "
                    f"candidate rewrote or discarded an existing entry")
        # Archiving MOVES a cohort out of `current` into `history`, so
        # the append-only check above cannot see a cohort that was
        # dropped instead. Retained measurements only ever grow.
        if _sample_total(now) < _sample_total(was):
            problems.append(
                f"probe {key!r} lost retained measurements "
                f"({_sample_total(was)} before, {_sample_total(now)} after)")
    return problems


def _refuse_symlink(path: Path) -> None:
    """A symlinked target or directory is refused, never followed.

    `os.replace` replaces the LINK, so following one would write the
    census wherever the link points — the primary checkout included,
    which is exactly what this tool must never touch.
    """
    for candidate in (path, path.parent):
        if candidate.is_symlink():
            raise CensusError(
                f"refusing to use {candidate}: a census path may not be a "
                f"symlink")


def read_for_update(path: Path) -> dict | None:
    """The current census as `probe-census/v2`, or None if absent.

    Malformed state is a `CensusError` here rather than something the
    writer repairs: a census nobody can read is a stop, and the primary
    checkout is never involved either way.
    """
    _refuse_symlink(path)
    if not path.exists():
        return None
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except OSError as error:
        raise CensusError(f"census {path} is unreadable ({error})") from None
    except ValueError as error:
        raise CensusError(
            f"census {path} is not valid JSON: {error}") from None
    return migrate_document(document)


def update(path: Path, mutate) -> dict:
    """One locked read-modify-write of the census at `path`.

    `mutate` receives the migrated document (or None when the target does
    not exist yet) and returns `(candidate, touched)`, where `touched` is
    the set of probe keys it is allowed to have changed or `TOUCH_ANY`
    for an inventory operation. The lock is held from the read through
    validation to the replacement, and the bytes that are validated are
    exactly the bytes installed.
    """
    path = Path(path)
    with _locked(path):
        before = read_for_update(path)
        candidate, touched = mutate(before)
        payload = render_manifest(candidate).encode("utf-8")
        # Validate the SERIALIZED bytes: what a later reader will see,
        # not the in-memory object that produced them.
        installed = json.loads(payload.decode("utf-8"))
        problems = validate_structure(installed)
        problems += _check_preserved(before, installed, touched)
        if problems:
            raise CensusError("refusing to install a malformed census: " +
                              "; ".join(problems[:5]))
        _clear_staging(path.parent)
        _atomic_replace(path, payload)
        return installed


def ensure_document(path: Path) -> dict:
    """Create, migrate, or refresh the census at `path`, losing nothing.

    An ABSENT target gets a fresh seed. An existing one is migrated to
    the current schema and reconciled against the live registry — never
    regenerated, so accumulated census data cannot be overwritten by a
    freshly generated inventory.
    """
    def mutate(before):
        if before is None:
            return build_manifest(), TOUCH_ANY
        return reconcile_inventory(before), TOUCH_ANY
    return update(path, mutate)


def record_result(path: Path, result) -> str:
    """Ingest one `probe-flake-result/v1` document. Returns the probe."""
    touched: list[str] = []

    def mutate(before):
        if before is None:
            raise CensusError(
                f"census {path} does not exist yet; seed it first with "
                f"`python3 tools/probe_census.py --seed`")
        candidate, probe = ingest_result(before, result)
        touched.append(probe)
        return candidate, {probe}
    update(path, mutate)
    return touched[0]


def record_policy(path: Path, probe: str, **fields) -> str:
    def mutate(before):
        if before is None:
            raise CensusError(
                f"census {path} does not exist yet; seed it first with "
                f"`python3 tools/probe_census.py --seed`")
        candidate, key = set_policy(before, probe, **fields)
        return candidate, {key}
    update(path, mutate)
    return probe


# Kept for callers of #1425's seeding entry point. It no longer
# regenerates over an existing document.
def seed(repo_root: str | None = None) -> Path:
    path = manifest_path(repo_root)
    ensure_document(path)
    return path


# ==========================================================================
# CLI
# ==========================================================================
def _parse_optional_int(text: str, what: str) -> int | None:
    if text.lower() in ("none", "null", ""):
        return None
    try:
        value = int(text)
    except ValueError:
        raise CensusError(f"{what} must be an integer or `none`, got "
                          f"{text!r}") from None
    if value < 0:
        raise CensusError(f"{what} must not be negative, got {value}")
    return value


def _parse_optional_float(text: str, what: str) -> float | None:
    if text.lower() in ("none", "null", ""):
        return None
    try:
        value = float(text)
    except ValueError:
        raise CensusError(f"{what} must be a number or `none`, got "
                          f"{text!r}") from None
    if not math.isfinite(value) or value < 0:
        raise CensusError(f"{what} must be finite and non-negative, got {text!r}")
    return value


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    group = ap.add_mutually_exclusive_group(required=True)
    group.add_argument("--print", dest="do_print", action="store_true",
                       help="print the manifest the live registry implies")
    group.add_argument("--seed", action="store_true",
                       help=f"create or migrate {MANIFEST_RELPATH} in the "
                            f"{DOCS_BRANCH} worktree, never overwriting census "
                            f"data")
    group.add_argument("--validate", action="store_true",
                       help=f"validate the {DOCS_BRANCH} worktree's census")
    group.add_argument("--record", metavar="RESULT",
                       help=f"ingest a {RESULT_SCHEMA} document")
    group.add_argument("--show", action="store_true",
                       help="print one probe's census record")
    group.add_argument("--set-acceptable-failures", metavar="N",
                       help="store X for --probe (an integer, or `none`)")
    group.add_argument("--set-estimate", metavar="SECONDS",
                       help="store the estimated worst-case duration for "
                            "--probe (a number of seconds, or `none`)")
    ap.add_argument("--probe", help="the probe key --show and --set-* act on")
    ap.add_argument("--justification", default=None,
                    help="the optional justification stored beside X")
    args = ap.parse_args(argv)

    if args.do_print:
        sys.stdout.write(render_manifest())
        return 0
    try:
        path = manifest_path()
        if args.seed:
            document = ensure_document(path)
            print(f"census at {path}: {len(document['probes'])} probes "
                  f"({CENSUS_SCHEMA})")
            return 0
        if args.record:
            try:
                result = json.loads(Path(args.record).read_text(encoding="utf-8"))
            except OSError as error:
                raise CensusError(f"cannot read {args.record} ({error})") from None
            except ValueError as error:
                raise CensusError(
                    f"{args.record} is not valid JSON: {error}") from None
            probe = record_result(path, result)
            print(f"recorded a {result.get('status')} measurement for "
                  f"{probe} in {path}")
            return 0
        if args.show or args.set_acceptable_failures is not None \
                or args.set_estimate is not None:
            if not args.probe:
                raise CensusError("--probe is required for this operation")
            if args.show:
                document = read_for_update(path)
                if document is None:
                    raise CensusError(f"census {path} does not exist yet")
                entry = find_entry(document, args.probe)
                if entry is None:
                    raise CensusError(
                        f"probe {args.probe!r} has no census entry")
                sys.stdout.write(json.dumps(entry, indent=2, sort_keys=True)
                                 + "\n")
                return 0
            fields: dict = {}
            if args.set_acceptable_failures is not None:
                fields["acceptable_failures"] = _parse_optional_int(
                    args.set_acceptable_failures, "--set-acceptable-failures")
                fields["justification"] = args.justification
            if args.set_estimate is not None:
                fields["estimate"] = _parse_optional_float(
                    args.set_estimate, "--set-estimate")
            record_policy(path, args.probe, **fields)
            print(f"updated the census record for {args.probe} in {path}")
            return 0

        document = load(path)
        if isinstance(document, dict) and document.get("schema") == SEED_SCHEMA:
            raise CensusError(
                f"{path} is still {SEED_SCHEMA}; migrate it in place with "
                f"`python3 tools/probe_census.py --seed`, which never "
                f"overwrites census data")
        problems = validate_structure(document)
        problems += validate_manifest(document)
    except DocsWorktreeMissing as error:
        print(f"probe_census: {error}", file=sys.stderr)
        return 2
    except CensusError as error:
        print(f"probe_census: {error}", file=sys.stderr)
        return 1
    except ValueError as error:
        print(f"probe_census: {error}", file=sys.stderr)
        return 1
    if problems:
        for problem in problems:
            print(f"probe_census: {problem}", file=sys.stderr)
        return 1
    print(f"{path}: {len(run_probes.PROBES)} probes, inventory agrees with "
          f"run_probes.PROBES and tools/ci_probes.py")
    return 0


if __name__ == "__main__":
    sys.exit(main())
