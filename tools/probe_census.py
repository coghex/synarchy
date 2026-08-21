#!/usr/bin/env python3
"""The probe census: one durable record per probe, and its atomic writer.

`docs/probe_census.json` is the de-flake lab's durable record. #1425
seeded it as an INVENTORY — every registered probe exactly once, with
its script, its CI-eligible/manual-only classification, and its protocol
status (`legacy` or `probe-result/v1`). #1428 extends that SAME file to
`probe-census/v2` by adding the measurements the lab accumulates. There
is deliberately no second file and no manual-only-subset variant: the
inventory covers every registered probe, and only manual-only entries
accumulate current samples.

What a census record holds, for each probe:

* `acceptable_failures` — X, the nullable acceptable-failure count for a
  commit cohort, with an optional justification. This module STORES the
  supplied policy value; choosing it is #1430's job.
* `estimated_worst_case_seconds` — supplied metadata, deliberately
  distinct from the OBSERVED `worst_elapsed_seconds` of a sample.
* `current` — the current commit cohort: the cohort of the most recently
  accepted measurement commit. Another measurement for the SAME commit
  appends to it; a different commit archives the whole prior cohort into
  `history` first. No cohort or sample is ever overwritten or discarded.
* `history` — archived cohorts, append-only, retained forever. A probe
  promoted to CI eligibility keeps its history; it just stops receiving
  current samples.
* `attempts` — an append-only log of well-formed ingestion attempts. A
  well-formed harness-error result is logged but contributes no sample
  and no aggregate.

Only summarized outcomes and external artifact references live here.
Raw stdout, protocol event streams and engine logs stay in the
harness's artifact tree, outside every worktree.

DECLARED SCHEMA VALIDATION IS NOT HERE. This module reads only the
fields its own operations need, and refuses in a controlled way — no
traceback, authoritative bytes untouched — when it cannot safely perform
the requested action. Comprehensive shape/enum/range checking is #1492
(a declared JSON Schema through `jsonschema`) and the cross-field
invariants are #1493; a hand-edited document with a field removed,
retyped, or made cross-field-inconsistent is deliberately NOT this
module's problem.

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
processes writing the same census always contend and two different
censuses never do, and it is held from the initial read through
serialization and the preservation checks to the replacement.
Replacement writes a same-filesystem staging file and `os.replace`s it,
so every observer sees either the complete old document or the complete
new one — never a partial write, and never a stale staging file
promoted to authoritative.

Exit codes: 0 success; 2 a missing or unusable docs worktree (carrying
its actionable `git worktree add` message) and argparse's own usage
errors; 1 inventory drift and every controlled refusal.

Usage:
  python3 tools/probe_census.py --print            # the manifest, to stdout
  python3 tools/probe_census.py --seed             # create/migrate in docs-wip
  python3 tools/probe_census.py --validate         # check the docs-wip copy
  python3 tools/probe_census.py --record RESULT    # ingest one measurement
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
import stat
import subprocess
import sys
import tempfile
from contextlib import contextmanager
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ci_probes  # noqa: E402
import probe_flake  # noqa: E402
import run_probes  # noqa: E402

CENSUS_SCHEMA = "probe-census/v2"
SEED_SCHEMA = "probe-census/v1"
# The schema `--print`, `--seed` and `--validate` speak. Kept under the
# #1425 name too, because that is what the manifest helpers are called.
MANIFEST_SCHEMA = CENSUS_SCHEMA
MIGRATABLE_SCHEMAS = (SEED_SCHEMA, CENSUS_SCHEMA)

MANIFEST_RELPATH = "docs/probe_census.json"
DOCS_BRANCH = "docs-wip"

CI_ELIGIBLE = "ci-eligible"
MANUAL_ONLY = "manual-only"
LEGACY = "legacy"

RESULT_SCHEMA = probe_flake.RESULT_SCHEMA
RESULT_STATUSES = ("ok", "harness-error")

LOCK_SUFFIX = ".lock"
LOCK_NOTE = (b"tools/probe_census.py holds a cross-process flock on this "
             b"file while it rewrites docs/probe_census.json. It is "
             b"untracked scratch state; deleting it while no writer is "
             b"running is harmless.\n")
STAGING_PREFIX = ".probe_census."
STAGING_SUFFIX = ".tmp"

# `mutate` returns the probe keys it is allowed to have changed, or this
# sentinel for an inventory-wide operation (`--seed`), which may append
# rows but still may not reorder or drop one.
TOUCH_ANY = object()
# Distinguishes "leave this policy field alone" from "clear it to null".
KEEP = object()


class CensusError(Exception):
    """A controlled refusal: exit non-zero, leave the census bytes alone."""


class DocsWorktreeMissing(Exception):
    """No worktree is on `docs-wip`; the caller must create one."""


# ==========================================================================
# The record
# ==========================================================================
def classification(key: str) -> str:
    """The authoritative CI classification, read from `tools/ci_probes.py`."""
    return CI_ELIGIBLE if key in ci_probes.CI_ELIGIBLE else MANUAL_ONLY


def empty_census() -> dict:
    """A census record that has never been measured or given a policy."""
    return {
        "acceptable_failures": None,
        "acceptable_failures_justification": None,
        "estimated_worst_case_seconds": None,
        "current": None,
        "history": [],
        "attempts": [],
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
            for key, script, _purpose in run_probes.PROBES
        ],
    }


def render_manifest(manifest: dict | None = None) -> str:
    """The one deterministic, byte-stable serialization of a census.

    `sort_keys` is what makes the bytes a pure function of the content:
    the record blocks in #1428 enumerate required keys and their empty
    values, never a serialized key order.
    """
    try:
        return json.dumps(manifest if manifest is not None else build_manifest(),
                          indent=2, sort_keys=True) + "\n"
    except (TypeError, ValueError) as error:
        raise CensusError(f"census is not serializable as JSON: {error}") from None


def validate_manifest(manifest) -> list[str]:
    """Every disagreement between `manifest`'s INVENTORY and the registry.

    Inventory drift only: a missing, duplicate or extra entry, and any
    row whose script, classification or protocol status disagrees with
    `run_probes.PROBES`, `tools/ci_probes.py` and
    `probe_flake.PROTOCOL_PROBES`. Each row's `census` field is
    deliberately tolerated and never inspected — census-record shape is
    #1492's. A `probe-census/v1` document is reported here as schema
    drift and is NOT migrated as a side effect; `--seed` is the only
    operation that migrates.
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
    """A `probe-census/v1` or `/v2` document, as `/v2`.

    Lossless: every existing row survives, in the same order, with its
    existing inventory values. The v1 migration adds the exact empty
    census record; it never regenerates the inventory from the live
    registry, because the seeded document is migration INPUT.

    A v2 row missing its `census` field is left exactly as it is rather
    than repaired: in a v2 document that is corruption for #1492 to
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

    It never deletes a row and never rewrites a census record. A probe
    that left `run_probes.PROBES` keeps its measurements and is reported
    by `validate_manifest` as an extra entry, which is a decision for a
    person.
    """
    result = migrate_document(document)
    live = {key: script for key, script, _purpose in run_probes.PROBES}
    entries = [dict(entry) for entry in result["probes"]]
    present = {entry.get("key") for entry in entries}
    for entry in entries:
        key = entry.get("key")
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
def _deep_copy(value):
    """A copy no later mutation of the original can reach through."""
    return json.loads(json.dumps(value))


def find_entry(document, probe: str) -> dict | None:
    for entry in (document or {}).get("probes") or []:
        if isinstance(entry, dict) and entry.get("key") == probe:
            return entry
    return None


def result_target(result) -> tuple[str, str]:
    """The three fields `--record` must read to know what to do.

    The top-level `schema`, the `probe` whose row it mutates — the
    result document names its own row, which is why `--record` takes no
    `--probe` and why the durable sample omits `probe` — and the
    `status` that decides whether a sample is created. Any of the three
    absent or unrecognized blocks the operation; in particular an
    unrecognized `status` must NOT be logged as a non-accepted attempt.

    These are discriminators the operation itself needs, not the
    declared-schema validation surface deferred to #1492.
    """
    if not isinstance(result, dict):
        raise CensusError(
            f"a {RESULT_SCHEMA} document must be a JSON object, got "
            f"{type(result).__name__}")
    schema = result.get("schema")
    if schema != RESULT_SCHEMA:
        raise CensusError(
            f"result schema is {schema!r}, expected {RESULT_SCHEMA!r}")
    probe = result.get("probe")
    if not isinstance(probe, str) or not probe:
        raise CensusError(
            f"result names no probe: `probe` is {probe!r}")
    status = result.get("status")
    if status not in RESULT_STATUSES:
        raise CensusError(
            f"result status {status!r} is not one of {RESULT_STATUSES}")
    return probe, status


def _field(result, name: str):
    """One field the durable record is built from, or a refusal."""
    if name not in result:
        raise CensusError(
            f"a {RESULT_SCHEMA} document with no {name!r} cannot be recorded")
    return result[name]


def _artifact_list(result):
    values = _field(result, "retained_artifacts")
    if not isinstance(values, list):
        raise CensusError(
            f"`retained_artifacts` must be a list, got "
            f"{type(values).__name__}")
    return list(values)


def summarize_sample(result: dict) -> dict:
    """The durable record of one accepted measurement.

    Summarized outcomes and artifact REFERENCES only: the producer-only
    fields `Measurement.to_document` adds — `port`, the per-run `checks`
    map, the `checks` descriptor labels, `error_run`, `artifact_root`
    and `invocation_dir` — are deliberately dropped, and no stdout, no
    protocol stream and no engine log ever enters the census.
    """
    runs = _field(result, "runs")
    if not isinstance(runs, list):
        raise CensusError(f"`runs` must be a list, got {type(runs).__name__}")
    summarized = []
    for position, run in enumerate(runs):
        if not isinstance(run, dict):
            raise CensusError(f"run {position} is not an object")
        try:
            summarized.append({
                "index": run["index"],
                "outcome": run["outcome"],
                "elapsed_seconds": run["elapsed_seconds"],
                "artifact_dir": run.get("artifact_dir"),
            })
        except KeyError as error:
            raise CensusError(
                f"run {position} has no {error.args[0]!r} field") from None
    counts = _field(result, "check_counts")
    if not isinstance(counts, dict):
        raise CensusError(
            f"`check_counts` must be an object, got {type(counts).__name__}")
    return {
        "timestamp_utc": _field(result, "timestamp_utc"),
        "commit_sha": _field(result, "commit_sha"),
        "requested_runs": _field(result, "requested_runs"),
        "completed_runs": _field(result, "completed_runs"),
        "runs": summarized,
        "check_counts": _deep_copy(counts),
        "failure_count": _field(result, "failure_count"),
        "failure_rate": _field(result, "failure_rate"),
        "timeout_count": _field(result, "timeout_count"),
        "worst_elapsed_seconds": _field(result, "worst_elapsed_seconds"),
        "total_elapsed_seconds": _field(result, "total_elapsed_seconds"),
        "rts_capabilities": _field(result, "rts_capabilities"),
        "peak_concurrency": _field(result, "peak_concurrency"),
        "retained_artifacts": _artifact_list(result),
    }


def summarize_attempt(result: dict, accepted: bool) -> dict:
    """The durable record of one well-formed ingestion attempt."""
    return {
        "timestamp_utc": _field(result, "timestamp_utc"),
        "commit_sha": _field(result, "commit_sha"),
        "status": _field(result, "status"),
        "accepted": accepted,
        "requested_runs": _field(result, "requested_runs"),
        "completed_runs": _field(result, "completed_runs"),
        "error": result.get("error"),
        "retained_artifacts": _artifact_list(result),
    }


def ingest_result(document: dict, result) -> tuple[dict, str]:
    """`document` with `result` recorded, plus the probe row it touched.

    A `status: "ok"` measurement appends one durable sample and one
    accepted attempt; a well-formed harness error appends only a
    non-accepted attempt and touches neither `current` nor any
    aggregate. An accepted measurement naming the same commit as
    `current` appends to that cohort; a different commit archives the
    complete prior cohort into `history` first.

    Deliberately NOT idempotent: recording the same document twice
    appends a second sample and a second attempt. The record is
    append-only and this slice introduces no deduplication key.
    """
    probe, status = result_target(result)
    entries = _rows(document, "census")
    target = find_entry(document, probe)
    if target is None:
        # Distinct from the inventory-parity rule: an unrelated missing,
        # added or stale row never refuses a finished measurement. Only
        # the absence of the row being written refuses, because there is
        # nowhere to append without fabricating inventory `--seed` owns.
        raise CensusError(
            f"probe {probe!r} has no census row; reconcile the inventory "
            f"with `python3 tools/probe_census.py --seed`")
    if not isinstance(target.get("census"), dict):
        raise CensusError(
            f"probe {probe!r} has no census record to append to")
    accepted = status == "ok"
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


def set_policy(document: dict, probe: str, *,
               acceptable_failures=KEEP, justification=KEEP,
               estimate=KEEP) -> tuple[dict, str]:
    """Store the supplied X / justification / estimate for one probe.

    `KEEP` leaves a field alone; `None` clears it. Clearing
    `acceptable_failures` also clears its justification, because a
    justification with nothing to justify is not a state this record
    can hold. Every unrelated policy field and all measurement history
    are left exactly as they were.

    This module stores the policy it is given: range and
    justification-policy enforcement are #1430's and #1492's.
    """
    entries = _rows(document, "census")
    target = find_entry(document, probe)
    if target is None:
        raise CensusError(f"probe {probe!r} has no census row")
    if not isinstance(target.get("census"), dict):
        raise CensusError(f"probe {probe!r} has no census record to update")
    if acceptable_failures is not KEEP and acceptable_failures is None:
        justification = None
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


# ==========================================================================
# The docs worktree
# ==========================================================================
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


def _refuse_substituted(path: Path, what: str) -> None:
    """A symlinked, hard-linked or non-regular census path is refused.

    All three resolved paths get this — the census target, its
    directory, and the lock — not the lock alone. `os.replace` replaces
    the LINK, so following a symlinked `probe_census.json` would write
    the census wherever the link points (the primary checkout included),
    and replacing a hard-linked target silently strands the other name
    on the old bytes. Either defeats "leave the old authoritative bytes
    unchanged".
    """
    if path.parent.is_symlink():
        raise CensusError(
            f"refusing to use {path.parent}: the {what} directory may not be "
            f"a symlink")
    try:
        info = os.lstat(path)
    except FileNotFoundError:
        return
    except OSError as error:
        raise CensusError(f"could not stat the {what} {path} ({error})") from None
    if stat.S_ISLNK(info.st_mode):
        raise CensusError(
            f"refusing to use {path}: the {what} may not be a symlink")
    if not stat.S_ISREG(info.st_mode):
        raise CensusError(
            f"refusing to use {path}: the {what} must be a regular file "
            f"(got mode {stat.S_IFMT(info.st_mode):#o})")
    if info.st_nlink != 1:
        raise CensusError(
            f"refusing to use {path}: the {what} must have exactly one link "
            f"(got {info.st_nlink})")


@contextmanager
def _locked(target: Path):
    _refuse_substituted(Path(target), "census")
    guard = lock_path(target)
    _refuse_substituted(guard, "census lock")
    try:
        # The census directory is created here rather than at
        # replacement time so the lock exists for the very first writer
        # too. Reaching this point already means a `docs-wip` worktree
        # resolved, so nothing is created anywhere else.
        guard.parent.mkdir(parents=True, exist_ok=True)
        # O_NOFOLLOW closes the race between the lstat above and this
        # open: a lock path swapped for a symlink in between must fail
        # rather than be followed, or the note below would land wherever
        # it points — the primary checkout included.
        fd = os.open(str(guard), os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
    except OSError as error:
        raise CensusError(
            f"could not open the census lock {guard} ({error})") from None
    # Checked and refused BEFORE the lock is taken, so the failure is not
    # routed through an unlock this file may not support.
    try:
        info = os.fstat(fd)
    except OSError as error:
        os.close(fd)
        raise CensusError(
            f"could not stat the census lock {guard} ({error})") from None
    # `st_nlink == 1` as well as regular: `O_NOFOLLOW` stops a SYMLINK,
    # but a HARD LINK planted at the lock path is the same inode as some
    # file elsewhere, and the note below would be written into it.
    if not stat.S_ISREG(info.st_mode) or info.st_nlink != 1:
        os.close(fd)
        raise CensusError(
            f"refusing to use {guard}: the census lock must be a regular file "
            f"with exactly one link (got mode {stat.S_IFMT(info.st_mode):#o}, "
            f"{info.st_nlink} links)")
    try:
        fcntl.flock(fd, fcntl.LOCK_EX)
        # The lock file is deliberately never unlinked: removing a HELD
        # flock file lets the next writer create a fresh inode, lock
        # that, and lose an update. It is a small untracked file in the
        # docs worktree — which is where CLAUDE.md's working-tree
        # discipline says an uncommitted file belongs — so it says so
        # itself for whoever finds it in `git status`. Re-stat under the
        # lock: another writer may have filled it since the open.
        if os.fstat(fd).st_size == 0:
            os.write(fd, LOCK_NOTE)
        yield
    finally:
        try:
            fcntl.flock(fd, fcntl.LOCK_UN)
        except OSError:
            # Closing the descriptor releases the lock regardless, and a
            # failing unlock must never mask the error that got us here.
            pass
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
        # `mkstemp` creates with O_EXCL, so the staging path cannot be a
        # pre-planted symlink; assert the resulting inode anyway, since
        # this is the third path the substitution rule names.
        info = os.fstat(fd)
        if not stat.S_ISREG(info.st_mode) or info.st_nlink != 1:
            raise CensusError(
                f"refusing to use {staged_path}: the census staging file must "
                f"be a regular file with exactly one link")
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


# ==========================================================================
# The preservation contract
# ==========================================================================
def _entry_map(document) -> dict:
    return {entry["key"]: entry for entry in (document or {}).get("probes") or []
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


POLICY_FIELDS = ("acceptable_failures", "acceptable_failures_justification",
                 "estimated_worst_case_seconds")
MEASUREMENT_FIELDS = ("current", "history", "attempts")
INVENTORY_FIELDS = ("key", "script", "classification", "protocol")


def _census_of(entry) -> dict:
    census = (entry or {}).get("census")
    return census if isinstance(census, dict) else {}


def _append_only(key: str, was: dict, now: dict) -> list[str]:
    """`history` and `attempts` grew by appending, and nothing was lost."""
    problems: list[str] = []
    for field in ("history", "attempts"):
        previous = was.get(field) or []
        current = now.get(field) or []
        if current[:len(previous)] != previous:
            problems.append(
                f"probe {key!r} `{field}` is append-only, but the candidate "
                f"rewrote or discarded an existing entry")
    # Archiving MOVES a cohort out of `current` into `history`, so the
    # append-only check above cannot see a cohort that was dropped
    # instead. Retained measurements only ever grow.
    if _sample_total(now) < _sample_total(was):
        problems.append(
            f"probe {key!r} lost retained measurements "
            f"({_sample_total(was)} before, {_sample_total(now)} after)")
    return problems


def _check_preserved(before, after, touched) -> list[str]:
    """Every way a candidate disturbed what it had no business touching.

    JSON serialization necessarily rewrites the whole file, so this is
    what makes "changes only the affected probe's record" real. It
    compares the candidate against the document it would replace and
    knows nothing about field shapes — this is the issue's preservation
    contract, not the schema validation deferred to #1492.

    `touched` maps a probe key to the aspects its operation may change
    (`"policy"`, `"measurements"`), or is `TOUCH_ANY` for `--seed`.
    Everything else must come through untouched: an unrelated row
    deeply equal and in the same position, a measurement leaving policy
    alone, a policy update leaving every cohort, sample and attempt
    alone, and no operation at all shrinking the retained measurements.
    """
    if before is None:
        return []
    problems: list[str] = []
    inventory = touched is TOUCH_ANY
    before_keys = [e.get("key") for e in (before.get("probes") or [])
                   if isinstance(e, dict)]
    after_keys = [e.get("key") for e in (after.get("probes") or [])
                  if isinstance(e, dict)]
    if inventory:
        # `--seed` may APPEND newly registered probes; it may not
        # reorder or drop one.
        if after_keys[:len(before_keys)] != before_keys:
            return ["the candidate reordered or dropped existing inventory "
                    "entries"]
    elif after_keys != before_keys:
        return [f"the candidate changed the inventory order or membership "
                f"({len(before_keys)} entries before, {len(after_keys)} after)"]

    old = _entry_map(before)
    new = _entry_map(after)
    for key, entry in old.items():
        candidate = new.get(key)
        if candidate is None:
            problems.append(f"the candidate dropped probe {key!r}")
            continue
        was, now = _census_of(entry), _census_of(candidate)
        if inventory:
            # Reconciliation refreshes inventory columns and may archive
            # a cohort on CI promotion. It never touches policy and
            # never loses a measurement.
            problems += _append_only(key, was, now)
            for field in POLICY_FIELDS:
                if now.get(field) != was.get(field):
                    problems.append(
                        f"probe {key!r}: reconciliation changed policy field "
                        f"`{field}`")
            continue
        allowed = set(touched.get(key) or ())
        if not allowed:
            if candidate != entry:
                problems.append(
                    f"the candidate modified unrelated probe {key!r}")
            continue
        for field in INVENTORY_FIELDS:
            if candidate.get(field) != entry.get(field):
                problems.append(
                    f"probe {key!r}: the candidate changed inventory field "
                    f"`{field}`, which only --seed may refresh")
        if "policy" not in allowed:
            for field in POLICY_FIELDS:
                if now.get(field) != was.get(field):
                    problems.append(
                        f"probe {key!r}: the candidate changed policy field "
                        f"`{field}`")
        if "measurements" in allowed:
            problems += _append_only(key, was, now)
        else:
            for field in MEASUREMENT_FIELDS:
                if now.get(field) != was.get(field):
                    problems.append(
                        f"probe {key!r}: the candidate changed `{field}`, "
                        f"which a policy update may not touch")
    return problems


def read_for_update(path: Path):
    """The census exactly as stored, or None when it does not exist.

    Unreadable or non-JSON state is a controlled refusal here rather
    than something the writer repairs, and the document is returned
    UNMIGRATED so each operation can decide for itself: only `--seed`
    migrates.
    """
    _refuse_substituted(Path(path), "census")
    if not path.exists():
        return None
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except OSError as error:
        raise CensusError(f"census {path} is unreadable ({error})") from None
    except ValueError as error:
        raise CensusError(
            f"census {path} is not valid JSON: {error}") from None


def require_current_schema(document, path: Path) -> dict:
    """The census as a v2 document, or the refusal that names `--seed`.

    `--record` and the policy operations never migrate and never seed:
    an absent or still-v1 census is a controlled stop naming the one
    operation that fixes it. No measurement is lost by the refusal —
    `probe_flake --result PATH` writes the result document to an
    external path, so the operator seeds and re-runs `--record` on the
    same file.
    """
    if document is None:
        raise CensusError(
            f"census {path} does not exist yet; create it with "
            f"`python3 tools/probe_census.py --seed`")
    if not isinstance(document, dict):
        raise CensusError(
            f"census {path} must be a JSON object, got "
            f"{type(document).__name__}")
    schema = document.get("schema")
    if schema != CENSUS_SCHEMA:
        raise CensusError(
            f"census {path} is {schema!r}, not {CENSUS_SCHEMA!r}; migrate it "
            f"in place with `python3 tools/probe_census.py --seed`, which "
            f"never overwrites census data")
    return document


def update(path: Path, mutate) -> dict:
    """One locked read-modify-write of the census at `path`.

    `mutate` receives the stored document (or None when the target does
    not exist yet) and returns `(candidate, touched)`, where `touched`
    is the set of probe keys it is allowed to have changed or
    `TOUCH_ANY` for an inventory operation. The lock is held from the
    read through serialization and the preservation checks to the
    replacement, and the bytes that are checked are exactly the bytes
    installed.

    Any failure before `os.replace` — a refusing mutation, an
    unserializable candidate, a preservation violation, a staging write
    that dies — leaves the old authoritative bytes untouched.
    """
    path = Path(path)
    with _locked(path):
        before = read_for_update(path)
        try:
            candidate, touched = mutate(before)
            payload = render_manifest(candidate).encode("utf-8")
        except CensusError:
            raise
        except (TypeError, ValueError, KeyError, AttributeError,
                IndexError) as error:
            # The safety boundary the issue requires, at the ONE funnel
            # every mutation passes through: a structural or type error
            # met while performing the operation becomes a controlled
            # refusal instead of a traceback. It is not schema
            # validation — it reports only what actually blocked this
            # operation, and #1492/#1493 still own finding the rest.
            raise CensusError(
                f"census {path} is structurally malformed for this operation "
                f"({type(error).__name__}: {error})") from None
        # Compare against the bytes a later reader will see, not the
        # in-memory object that produced them.
        installed = json.loads(payload.decode("utf-8"))
        problems = _check_preserved(before, installed, touched)
        if problems:
            raise CensusError("refusing to install a census that loses data: " +
                              "; ".join(problems[:5]))
        _clear_staging(path.parent)
        if path.exists() and path.read_bytes() == payload:
            # A drift-free `--seed` is genuinely a no-op: leave the file,
            # its inode and its mtime exactly as they are.
            return installed
        _atomic_replace(path, payload)
        return installed


def ensure_document(path: Path) -> dict:
    """Create, migrate, or reconcile the census at `path`, losing nothing.

    An ABSENT target gets a fresh v2 seed. An existing one is migrated
    to the current schema and reconciled against the live registry —
    never regenerated, so accumulated census data cannot be overwritten
    by a freshly generated inventory.
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
        document = require_current_schema(before, path)
        candidate, probe = ingest_result(document, result)
        touched.append(probe)
        return candidate, {probe: {"measurements"}}
    update(path, mutate)
    return touched[0]


def record_policy(path: Path, probe: str, **fields) -> str:
    def mutate(before):
        document = require_current_schema(before, path)
        candidate, key = set_policy(document, probe, **fields)
        return candidate, {key: {"policy"}}
    update(path, mutate)
    return probe


def seed(repo_root: str | None = None) -> Path:
    """#1425's entry point. It no longer regenerates over an existing file."""
    path = manifest_path(repo_root)
    ensure_document(path)
    return path


# ==========================================================================
# CLI
# ==========================================================================
def _optional_int(text: str, what: str) -> int | None:
    """`none`, or an integer. Range policy is #1430's, not this tool's."""
    if text == "none":
        return None
    try:
        return int(text)
    except ValueError:
        raise CensusError(
            f"{what} takes an integer or the literal `none`, got "
            f"{text!r}") from None


def _optional_number(text: str, what: str) -> float | int | None:
    """`none`, or a finite number. An integral token stays an integer."""
    if text == "none":
        return None
    try:
        return int(text)
    except ValueError:
        pass
    try:
        value = float(text)
    except ValueError:
        raise CensusError(
            f"{what} takes a number or the literal `none`, got "
            f"{text!r}") from None
    if not math.isfinite(value):
        # JSON has no NaN or Infinity, so storing one would make the
        # census unreadable to every other reader.
        raise CensusError(f"{what} must be finite, got {text!r}")
    return value


def _companion_arguments(args) -> dict | None:
    """The `set_policy` keywords the CLI arguments select, or None.

    EVERY argument-combination error lands here, and this runs before
    any operation dispatches — including `--print`, which would
    otherwise let `--probe`/`--justification` through unchecked simply
    by returning early. Nothing here reads or writes the census, and
    nothing here resolves the docs worktree.
    """
    setting_x = args.set_acceptable_failures is not None
    setting_estimate = args.set_estimate is not None
    policy = setting_x or setting_estimate
    # `is not None`, not truthiness: `--probe ""` was still supplied.
    if args.probe is not None and not policy:
        raise CensusError(
            "--probe is only used by --set-acceptable-failures and "
            "--set-estimate")
    if args.justification is not None and not setting_x:
        raise CensusError(
            "--justification is only valid with --set-acceptable-failures")
    if not policy:
        return None
    if setting_x and setting_estimate:
        raise CensusError(
            "--set-acceptable-failures and --set-estimate update different "
            "policy fields; use one per invocation")
    if not args.probe:
        raise CensusError("--probe KEY is required for a policy update")
    if setting_x:
        return {
            "acceptable_failures": _optional_int(
                args.set_acceptable_failures, "--set-acceptable-failures"),
            "justification": (KEEP if args.justification is None else
                              (None if args.justification == "none"
                               else args.justification)),
        }
    return {"estimate": _optional_number(args.set_estimate, "--set-estimate")}


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    group = ap.add_mutually_exclusive_group(required=True)
    group.add_argument("--print", dest="do_print", action="store_true",
                       help="print the census the live registry implies")
    group.add_argument("--seed", action="store_true",
                       help=f"create or migrate {MANIFEST_RELPATH} in the "
                            f"{DOCS_BRANCH} worktree, never overwriting "
                            f"census data")
    group.add_argument("--validate", action="store_true",
                       help=f"check the {DOCS_BRANCH} worktree's inventory")
    group.add_argument("--record", metavar="RESULT",
                       help=f"ingest one {RESULT_SCHEMA} document")
    group.add_argument("--set-acceptable-failures", metavar="N",
                       help="store X for --probe (an integer, or `none`)")
    group.add_argument("--set-estimate", metavar="SECONDS",
                       help="store the estimated worst-case duration for "
                            "--probe (a number of seconds, or `none`)")
    ap.add_argument("--probe", help="the probe key a policy update acts on")
    ap.add_argument("--justification", default=None,
                    help="the justification stored beside X (or `none` to "
                         "clear it); omit to leave it unchanged")
    args = ap.parse_args(argv)

    # Argument validation runs FIRST, for every operation. `--print`
    # returns without touching the filesystem, but it must not be a hole
    # through which a misused companion flag passes unreported.
    try:
        fields = _companion_arguments(args)
    except CensusError as error:
        print(f"probe_census: {error}", file=sys.stderr)
        return 1

    # `--print` must never require, read or create the docs worktree:
    # that is what lets a fresh checkout run it.
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
                result = json.loads(
                    Path(args.record).read_text(encoding="utf-8"))
            except OSError as error:
                raise CensusError(
                    f"cannot read {args.record} ({error})") from None
            except ValueError as error:
                raise CensusError(
                    f"{args.record} is not valid JSON: {error}") from None
            probe = record_result(path, result)
            print(f"recorded a {result.get('status')} measurement for "
                  f"{probe} in {path}")
            return 0
        if fields is not None:
            record_policy(path, args.probe, **fields)
            print(f"updated the census record for {args.probe} in {path}")
            return 0
        problems = validate_manifest(load(path))
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
