#!/usr/bin/env python3
"""The census contract: what a document IS, and what makes one valid (#2131).

`tools/probe_census.py` is the command and the compatibility facade;
this is the bottom of its module stack. It answers one question — is
this document, or this result, admissible — and it answers it without
reading the filesystem, taking a lock, mutating a census or rendering
anything. Nothing here imports another census owner, which is what makes
the rest of the stack acyclic: records, summary, storage and promotion
all depend on this module and it depends on none of them.

What it owns:

* the schema identifiers and the shared contract constants every other
  owner spells its refusals against — the census and result schema
  strings, the migratable set, the classification names, the
  acceptable-failure policy's fixed N and its admissible X range, and
  the three tolerance states;
* `CensusError`, the controlled-refusal type the whole stack raises;
* the declared JSON Schema (#1492): loading it from `probe_census_schema.json`
  beside this file, caching the validators, and refusing an environment
  without `jsonschema` loudly rather than silently enforcing nothing;
* the non-finite-number rejection the declared schema cannot express,
  because JSON Schema has no way to say "not NaN";
* the cross-field invariants (#1493) for a result and for a stored
  census — the rules that SPAN fields, which no keyword expresses;
* the scalar requirements every consumer states its inputs against:
  `require_commit_identity`, `parse_timestamp`, `require_count` and
  `require_measurement_semantics`;
* `cohort_statistic`, one cohort's combined statistic and its freshness
  anchor.

Those last two groups sit HERE rather than with the summary reader, and
that placement is load-bearing rather than tidy. `ingest_result`
(records) needs `require_measurement_semantics` and `cohort_statistic`,
while `summarize_entry` (summary) needs `policy_sample` and
`tolerance_state` (records). Split those primitives between records and
summary and the two owners import each other; keeping them at the
bottom, where they belong anyway — they depend on nothing but
`CensusError`, each other and `TIMESTAMP_FORMAT` — leaves the one
direction records → summary that the stack actually has.

This module has no CLI and is not a gate of its own. Every command is
still `python3 tools/probe_census.py`.
"""
from __future__ import annotations

import datetime
import json
import math
import os
import sys
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_flake  # noqa: E402
import probe_protocol  # noqa: E402


CENSUS_SCHEMA = "probe-census/v5"
OUTCOME_SCHEMA = "probe-census/v4"
CLAIM_SCHEMA = "probe-census/v3"
RECORD_SCHEMA = "probe-census/v2"
SEED_SCHEMA = "probe-census/v1"
# The schema `--print`, `--seed` and `--validate` speak. Kept under the
# #1425 name too, because that is what the manifest helpers are called.
MANIFEST_SCHEMA = CENSUS_SCHEMA
MIGRATABLE_SCHEMAS = (SEED_SCHEMA, RECORD_SCHEMA, CLAIM_SCHEMA,
                      OUTCOME_SCHEMA, CENSUS_SCHEMA)

CI_ELIGIBLE = "ci-eligible"
MANUAL_ONLY = "manual-only"
LEGACY = "legacy"

# --------------------------------------------------------------------------
# The acceptable-failure policy (#1430)
# --------------------------------------------------------------------------
# X is how many failures a COMPLETE N-run measurement may show and still
# be acceptable. N is fixed at ten, so the admissible values are 0
# through 9: an X of ten would declare a probe that fails every single
# run acceptable, which is not a tolerance but a surrender.
POLICY_RUN_COUNT = 10
MIN_ACCEPTABLE_FAILURES = 0
MAX_ACCEPTABLE_FAILURES = POLICY_RUN_COUNT - 1
# X=0 is the default and the only value that needs no argument: a probe
# must pass every run until a maintainer states, in writing, why some
# failures are acceptable.
DEFAULT_ACCEPTABLE_FAILURES = 0

# Where one complete ten-run measurement sits against a record's own X.
TOLERANCE_ACCEPTABLE = "acceptable"
TOLERANCE_OVER = "over-tolerance"
# Either the record carries no usable X, or the measurement is not the
# fixed-N basis the policy is stated against. `probe_flake.py` accepts
# any positive `--runs`, so such a measurement stays valid data that
# this threshold simply does not classify.
TOLERANCE_NOT_COMPARABLE = "not-comparable"

RESULT_SCHEMA = probe_flake.RESULT_SCHEMA
# The admissible result statuses are the schema's `result_status`
# definition, not a second constant here: `ingest_result` asks only
# whether a validated status is the accepted one.
ACCEPTED_STATUS = "ok"

# The declared schema (#1492), beside this module rather than in the
# docs worktree: it is the tool's own contract, and `--print` on a fresh
# checkout must be able to reach it.
SCHEMA_PATH = Path(__file__).resolve().parent / "probe_census_schema.json"
INSTALL_HINT = "python3 -m pip install --user -r tools/requirements-assets.txt"
# Each declared schema string, and the `$defs` entry that describes it.
# The definitions cannot simply BE the schema strings: a JSON Pointer
# treats `/` as a separator, so `#/$defs/probe-census/v2` names nothing.
SCHEMA_DEFINITIONS = {
    SEED_SCHEMA: "census_v1",
    RECORD_SCHEMA: "census_v2",
    CLAIM_SCHEMA: "census_v3",
    OUTCOME_SCHEMA: "census_v4",
    CENSUS_SCHEMA: "census_v5",
    RESULT_SCHEMA: "flake_result_v1",
}


class CensusError(Exception):
    """A controlled refusal: exit non-zero, leave the census bytes alone."""


# ==========================================================================
# Declared schema validation (#1492)
# ==========================================================================
#
# One data file replaces the type, presence, closure, enum, length and
# range checking a census validator would otherwise accumulate by hand.
# The lesson is #1309's, recorded in CLAUDE.md after fourteen review
# rounds on a hand-rolled format parser: depend on a library, or narrow
# the promise. #1428 narrowed; this depends.
#
# Nothing here is cached across a missing dependency:
# `_require_jsonschema` runs at every entry point BEFORE the caches are
# consulted, so an environment without the library produces the same
# loud refusal every time rather than a validator that, having once
# worked, quietly enforces nothing.
_SCHEMA_CACHE: dict = {}


def _require_jsonschema():
    """The validator library, or the one loud error that names its install.

    A skipped check that prints a clean run is worse than no check at
    all, so this is a hard failure with an actionable command — the rule
    `pack_atlas.py` already applies to Pillow.
    """
    try:
        import jsonschema  # noqa: PLC0415 - deliberately imported at use
    except ImportError as error:
        raise CensusError(
            f"jsonschema is required to validate the census and could not be "
            f"imported ({error}). Install the pinned toolchain with:\n"
            f"  {INSTALL_HINT}") from None
    return jsonschema


def load_schema() -> dict:
    """`tools/probe_census_schema.json`, parsed and self-checked.

    The document must NAME its draft (`$schema`) and pass that draft's
    own meta-schema check, so a schema edit that is not a valid schema
    fails here rather than silently validating nothing: an unrecognized
    `$schema` would otherwise fall back to whichever draft the installed
    library happens to consider newest.

    Every step is ordered so the NEXT one is safe to take. The root is
    proved to be an object and its `$schema` proved to be a string
    before `validator_for` is asked anything, because that helper
    subscripts what it is given: a valid-JSON schema file that is a list
    or a scalar would raise out of the library rather than refuse here,
    and this module's whole promise is that it does not do that.
    """
    jsonschema = _require_jsonschema()
    cached = _SCHEMA_CACHE.get("document")
    if cached is not None:
        return cached
    try:
        document = json.loads(SCHEMA_PATH.read_text(encoding="utf-8"))
    except OSError as error:
        raise CensusError(
            f"the census schema {SCHEMA_PATH} is unreadable ({error})") from None
    except ValueError as error:
        raise CensusError(
            f"the census schema {SCHEMA_PATH} is not valid JSON: "
            f"{error}") from None
    if not isinstance(document, dict):
        raise CensusError(
            f"the census schema {SCHEMA_PATH} must be a JSON object, got "
            f"{type(document).__name__}")
    declared = document.get("$schema")
    if not isinstance(declared, str):
        raise CensusError(
            f"the census schema {SCHEMA_PATH} does not identify a JSON Schema "
            f"draft (`$schema` is {declared!r})")
    factory = jsonschema.validators.validator_for(document, default=None)
    if factory is None:
        raise CensusError(
            f"the census schema {SCHEMA_PATH} names {declared!r}, which is not "
            f"a draft this jsonschema implements")
    try:
        factory.check_schema(document)
    except jsonschema.exceptions.SchemaError as error:
        raise CensusError(
            f"the census schema {SCHEMA_PATH} is not a valid {declared} "
            f"schema: {error.message}") from None
    _SCHEMA_CACHE["document"] = document
    return document


def _validator(definition: str):
    """A validator for one `$defs` entry, resolving `$ref`s in the file.

    The whole schema document is kept as the resource — only a root
    `$ref` is added — so every internal `#/$defs/...` reference still
    resolves against the same base.
    """
    jsonschema = _require_jsonschema()
    cached = _SCHEMA_CACHE.get(definition)
    if cached is not None:
        return cached
    document = load_schema()
    if definition not in (document.get("$defs") or {}):
        raise CensusError(
            f"the census schema {SCHEMA_PATH} declares no {definition!r} "
            f"definition")
    rooted = dict(document)
    rooted["$ref"] = f"#/$defs/{definition}"
    validator = jsonschema.validators.validator_for(document)(rooted)
    _SCHEMA_CACHE[definition] = validator
    return validator


def _non_finite_paths(node, prefix: str = "$") -> list[str]:
    """Every JSON path in `node` holding a NaN or an Infinity.

    `isinstance(True, float)` is False, so booleans are not walked as
    numbers.
    """
    if isinstance(node, float) and not math.isfinite(node):
        return [prefix]
    if isinstance(node, dict):
        found: list[str] = []
        for key, value in node.items():
            found += _non_finite_paths(value, f"{prefix}.{key}")
        return found
    if isinstance(node, list):
        found = []
        for index, value in enumerate(node):
            found += _non_finite_paths(value, f"{prefix}[{index}]")
        return found
    return []


def _reject_non_finite(document, what: str) -> None:
    """The one numeric hazard the declared bounds cannot express.

    `json.loads` accepts Python's non-standard `NaN`, `Infinity` and
    `-Infinity` spellings, so a hand-edited census really can hold one.
    JSON Schema's `minimum`/`maximum` do not catch a NaN either — every
    comparison with it is false, so it passes both bounds. The schema
    therefore owns MAGNITUDE and this owns REPRESENTABILITY; together
    they are what a `_float_safe` helper would have been, and
    `render_manifest`'s `allow_nan=False` is the matching guarantee on
    the way out.
    """
    offenders = _non_finite_paths(document)
    if offenders:
        raise CensusError(
            f"{what} holds a non-finite number at {offenders[0]}; JSON has no "
            f"NaN or Infinity, so it could never be read back")


def validate_document(document, schema_name: str, what: str) -> None:
    """`document` against one declared schema, or a controlled refusal.

    The refusal names the offending JSON path and the violated rule, and
    reports how many further violations were found without listing them:
    a hand-edited census is not an adversarial input, so the first
    actionable location is the useful diagnostic.
    """
    jsonschema = _require_jsonschema()
    definition = SCHEMA_DEFINITIONS.get(schema_name)
    if definition is None:
        raise CensusError(
            f"there is no declared schema named {schema_name!r} "
            f"(expected one of {tuple(SCHEMA_DEFINITIONS)})")
    _reject_non_finite(document, what)
    validator = _validator(definition)
    try:
        errors = list(validator.iter_errors(document))
    except Exception as error:  # noqa: BLE001 - see below
        # A checked-in schema that PARSES and self-checks but cannot be
        # APPLIED — an internal `$ref` naming nothing is the realistic
        # way — is a repository defect, and this module's promise is a
        # controlled refusal rather than a traceback out of a CI gate.
        # The library raises this through its reference resolver, whose
        # exception type is not part of its public surface, so the catch
        # is by position rather than by class. `test_probe_census.py`
        # applies every declared schema to a valid document, so the
        # shipped schema cannot reach here unnoticed.
        raise CensusError(
            f"the census schema {SCHEMA_PATH} could not be applied to "
            f"{what} ({type(error).__name__}: {error})") from None
    if not errors:
        return
    # `best_match` weighs the violations rather than reporting whichever
    # the walk reached first, so a document broken in several places
    # names the most specific one.
    best = jsonschema.exceptions.best_match(errors) or errors[0]
    more = ("" if len(errors) == 1
            else f" (and {len(errors) - 1} further violation(s))")
    raise CensusError(
        f"{what} does not match the declared {schema_name} schema: at "
        f"{best.json_path}, {best.message}{more}")


# ==========================================================================
# The cross-field invariants (#1493)
# ==========================================================================
# The declared schema (#1492) checks each field in isolation, which is
# what it is good at and what it cannot be extended past: the rules
# below SPAN fields, so no keyword expresses them and they need real
# code. Each rejects state no real run could have written — they are the
# census's own consistency model, not defensive guesswork — and each is
# grounded in a specific behaviour of the producer, `probe_flake.py`.
#
# They are deliberately not an exhaustive corruption detector. A
# hand-edited census is not an adversarial input class; the promise is
# that inconsistent state fails SAFELY, as a controlled refusal in front
# of the operation, rather than being rewritten and so persisted. The
# stored rules therefore run wherever a census is read
# (`validate_census`, so `--validate`, `--record`, `--seed` and the
# policy updates all stop on one) and again on the candidate, in the
# same two places the declared schema is applied.


def _zero_tally() -> dict:
    """One check's empty tally, in the producer's own vocabulary."""
    return {probe_protocol.PASS: 0, probe_protocol.FAIL: 0,
            probe_protocol.MISSING: 0}


def _render_tally(tally: dict) -> str:
    """A tally as one readable token, in a fixed outcome order."""
    return " ".join(f"{outcome}={tally.get(outcome, 0)}"
                    for outcome in (probe_protocol.PASS, probe_protocol.FAIL,
                                    probe_protocol.MISSING))


def observed_check_counts(runs) -> dict:
    """The check tally `runs` actually shows.

    `Measurement.check_counts()` tallies `self.runs` and nothing else,
    so `error_run` contributes nothing here either: it is not a member
    of `runs`, and a harness error is not a fourth probe outcome.

    Precondition: `runs` has already satisfied the declared schema, so
    every `checks` value is one of the three declared outcomes.
    """
    counts: dict = {}
    for run in runs:
        for check_id, outcome in run["checks"].items():
            counts.setdefault(check_id, _zero_tally())[outcome] += 1
    return counts


def _unfinished_measurement(status: str, completed: int, requested: int,
                            where: str) -> list[str]:
    """A non-accepted result may not claim it completed every run.

    A harness error ENDS the measurement: `stop_with_harness_error`
    returns before the broken run is appended to `runs`, so the run that
    broke the stream is never one of the completed ones and at least one
    requested run is always left uncompleted.
    """
    if completed < requested:
        return []
    return [f"at {where}, a {status!r} measurement reports completing "
            f"{completed} of {requested} requested run(s); the run that broke "
            f"the stream is never one of the completed ones, so a harness "
            f"error always leaves one uncompleted"]


def _rule_pass_run_has_no_failed_check(result) -> list[str]:
    """A PASS run may not carry a FAIL check.

    `reconcile` turns ANY failed check into a failing run — a FAIL check
    and a nonzero exit both land on `RUN_FAIL` — so this pairing is a
    contradiction no measurement produced. TIMEOUT is deliberately not
    covered: a timeout wins outright, whatever partial checks had
    already arrived, so a TIMEOUT run really can carry a FAIL.
    """
    problems: list[str] = []
    for position, run in enumerate(result["runs"]):
        if run["outcome"] != probe_flake.RUN_PASS:
            continue
        failed = sorted(check_id for check_id, outcome in run["checks"].items()
                        if outcome == probe_protocol.FAIL)
        if failed:
            problems.append(
                f"at $.runs[{position}].checks, run {run['index']} reports "
                f"{probe_flake.RUN_PASS} while carrying a "
                f"{probe_protocol.FAIL} check ({', '.join(failed)}); a failed "
                f"check makes its run fail")
    return problems


def _rule_check_counts_cover_the_descriptor(result) -> list[str]:
    """`check_counts` is keyed by exactly the descriptor's check ids.

    `Measurement.check_counts()` SEEDS the map from `descriptor.ids` and
    then only increments; it never adds a key and never drops one. So
    the keys are that id set exactly, whatever the runs did — including
    a measurement with no completed runs at all, whose entries are all
    zero. That is the half the per-entry tally cannot see: an id the
    descriptor never declared, carrying an all-zero tally, agrees with
    the runs (which show nothing for it) while being state no
    measurement could have produced.
    """
    declared = set(result["check_counts"])
    described = {check["id"] for check in result["checks"]}
    problems: list[str] = []
    for check_id in sorted(declared - described):
        problems.append(
            f"at $.check_counts.{check_id}, check {check_id!r} is tallied but "
            f"the probe's own descriptor does not declare it; `check_counts` "
            f"is keyed by exactly the declared checks")
    for check_id in sorted(described - declared):
        problems.append(
            f"at $.check_counts, declared check {check_id!r} has no tally; "
            f"`check_counts` is keyed by exactly the declared checks")
    return problems


def _rule_check_counts_tally_runs(result) -> list[str]:
    """Each entry is the tally `runs` shows, in both directions.

    The counts are DERIVED rather than reported, so any disagreement is
    an edit: a stored tally that is not what the runs show, and a check
    tallied in the runs with no entry at all, are each impossible.
    Keying is the neighbouring rule's; this one owns the numbers.
    """
    problems: list[str] = []
    observed = observed_check_counts(result["runs"])
    declared = result["check_counts"]
    for check_id in sorted(set(observed) | set(declared)):
        stored = declared.get(check_id)
        seen = observed.get(check_id, _zero_tally())
        if stored is None:
            problems.append(
                f"at $.check_counts, check {check_id!r} is tallied "
                f"{_render_tally(seen)} in `runs` but has no entry; "
                f"`check_counts` is the tally `runs` shows")
        elif stored != seen:
            problems.append(
                f"at $.check_counts.{check_id}, the stored tally "
                f"{_render_tally(stored)} is not the {_render_tally(seen)} "
                f"`runs` shows")
    return problems


def _rule_result_leaves_a_run_uncompleted(result) -> list[str]:
    """An incoming harness error never completed every requested run."""
    if result["status"] == ACCEPTED_STATUS:
        return []
    return _unfinished_measurement(
        result["status"], result["completed_runs"], result["requested_runs"],
        "$")


# The intake rules, in report order. The two `check_counts` rules are
# separate because they answer separate questions — which checks the map
# is keyed by, and what each entry counts — and neither implies the
# other: an undeclared all-zero entry satisfies the tally, and a wrong
# tally sits under a perfectly well-declared key.
RESULT_RULES = (
    _rule_pass_run_has_no_failed_check,
    _rule_check_counts_cover_the_descriptor,
    _rule_check_counts_tally_runs,
    _rule_result_leaves_a_run_uncompleted,
)


def result_invariants(result) -> list[str]:
    """Every cross-field violation in one intake document.

    Precondition: `result` has already satisfied the declared
    `probe-flake-result/v1` schema, so every field read here is present
    and of the declared type.
    """
    problems: list[str] = []
    for rule in RESULT_RULES:
        problems += rule(result)
    return problems


# --------------------------------------------------------------------------
# The stored record. Each rule takes the same `(census, where, name)` so
# the set can be composed — and so one can be lifted out of it, which is
# how `test_probe_census.py` proves each rule is the one doing the
# rejecting rather than a neighbour catching the same fixture.
# --------------------------------------------------------------------------
def _cohorts(census) -> list[tuple[str, dict]]:
    """The record's cohorts, labelled by where they are stored."""
    current = census["current"]
    cohorts = [] if current is None else [("current", current)]
    return cohorts + [(f"history[{index}]", cohort)
                      for index, cohort in enumerate(census["history"])]


def _accepted_attempts(census) -> int:
    """Attempts logging an INGESTED measurement, by their own status.

    Counted from `status` rather than from `accepted` because that is
    what `ingest_result` decides on; the two agreeing is a separate rule
    and keeping them separate is what lets each fail on its own case.
    """
    return sum(1 for attempt in census["attempts"]
               if attempt["status"] == ACCEPTED_STATUS)


def _rule_attempts_reconcile_with_samples(census, where, name) -> list[str]:
    """Accepted attempts and retained samples are one count.

    An accepted attempt is the LOG of an ingested measurement, and the
    sample is that measurement's retained form in `current` or in
    `history`, so the two are equal by construction. The lossy direction
    is the one worth naming — clearing the cohorts while leaving the
    accepted attempts reads as "these measurements were taken" with the
    measurements gone, and the next rewrite would persist that loss —
    but the opposite skew is equally impossible, so both are reported.

    Safe to enforce as an EQUALITY because nothing prunes or caps either
    collection: `_archive_current` MOVES the current cohort into
    `history`, `reconcile_inventory` promotes through that same move,
    and no operation deletes a row, a cohort, a sample or an attempt.
    """
    accepted = _accepted_attempts(census)
    retained = sum(len(cohort["samples"])
                   for _label, cohort in _cohorts(census))
    if accepted == retained:
        return []
    return [f"at {where}, {name} logs {accepted} accepted attempt(s) but "
            f"retains {retained} sample(s) across `current` and `history`; "
            f"every accepted attempt is one retained measurement, so these "
            f"cannot disagree"]


def _rule_accepted_derives_from_status(census, where, name) -> list[str]:
    """`accepted` is computed from `status`, so it cannot disagree."""
    problems: list[str] = []
    for position, attempt in enumerate(census["attempts"]):
        expected = attempt["status"] == ACCEPTED_STATUS
        if attempt["accepted"] is not expected:
            problems.append(
                f"at {where}.attempts[{position}], `accepted` is "
                f"{attempt['accepted']!r} beside status "
                f"{attempt['status']!r}; `accepted` is derived from "
                f"`status`, so it is {expected!r}")
    return problems


def _rule_attempt_leaves_a_run_uncompleted(census, where, name) -> list[str]:
    """A logged harness error never completed every requested run."""
    problems: list[str] = []
    for position, attempt in enumerate(census["attempts"]):
        if attempt["status"] == ACCEPTED_STATUS:
            continue
        problems += _unfinished_measurement(
            attempt["status"], attempt["completed_runs"],
            attempt["requested_runs"], f"{where}.attempts[{position}]")
    return problems


def _rule_cohort_holds_one_commit(census, where, name) -> list[str]:
    """A cohort IS one commit's samples.

    `ingest_result` opens a cohort named by the measurement's own commit
    and appends to it only measurements naming that same commit; a
    different commit archives the whole prior cohort first.
    """
    problems: list[str] = []
    for label, cohort in _cohorts(census):
        for position, sample in enumerate(cohort["samples"]):
            if sample["commit_sha"] != cohort["commit_sha"]:
                problems.append(
                    f"at {where}.{label}.samples[{position}], a sample from "
                    f"commit {sample['commit_sha']} sits in the "
                    f"{cohort['commit_sha']} cohort; a cohort holds one "
                    f"commit's samples")
    return problems


def _rule_deferral_is_actionable(census, where, name) -> list[str]:
    """A deferral says both why work stops and what makes it resumable."""
    deferred = census.get("deferred")
    if deferred is None:
        return []
    problems = []
    for field in ("reason", "resume_when"):
        value = deferred.get(field) if isinstance(deferred, dict) else None
        if not isinstance(value, str) or not value.strip():
            problems.append(
                f"at {where}.deferred.{field}, {name} has no non-blank "
                f"deferral {field.replace('_', ' ')}")
    return problems


# The stored rules, in report order: the whole-record reconciliation
# names data loss, so it leads.
CENSUS_RULES = (
    _rule_attempts_reconcile_with_samples,
    _rule_accepted_derives_from_status,
    _rule_attempt_leaves_a_run_uncompleted,
    _rule_cohort_holds_one_commit,
    _rule_deferral_is_actionable,
)


def census_record_invariants(census, where: str, probe) -> list[str]:
    """Every cross-field violation in one stored census record.

    Precondition: `census` has already satisfied the declared
    `census_record` schema.
    """
    name = f"probe {probe!r}" if probe is not None else "this record"
    problems: list[str] = []
    for rule in CENSUS_RULES:
        problems += rule(census, where, name)
    return problems


def census_invariants(document) -> list[str]:
    """Every cross-field violation in one stored census.

    A v1 seed carries no census records at all, so nothing here applies
    to one: its schema drift is `--validate`'s report and `--seed`'s
    repair, never corruption.

    Precondition: `document` has already satisfied the declared schema
    its own `schema` field names.
    """
    problems: list[str] = []
    for position, entry in enumerate(document.get("probes") or []):
        census = entry.get("census")
        if not isinstance(census, dict):
            continue
        problems += census_record_invariants(
            census, f"$.probes[{position}].census", entry.get("key"))
    return problems


def _refuse_inconsistent(problems: list[str], what: str) -> None:
    """A controlled refusal naming the first violation, or nothing.

    Reported the way `validate_document` reports a schema violation: the
    first actionable location, and a count of the rest. A hand-edited
    census is not an adversarial input, so listing every one of them is
    noise rather than diagnosis.
    """
    if not problems:
        return
    more = ("" if len(problems) == 1
            else f" (and {len(problems) - 1} further violation(s))")
    raise CensusError(
        f"{what} is internally inconsistent: {problems[0]}{more}")


def validate_result(result) -> None:
    """One `probe-flake-result/v1` document, before any field is read.

    Shape first, then the cross-field invariants (#1493) — which read
    nested fields, so they may only run once the declared schema has
    promised those fields are there and are what they claim to be.
    """
    what = f"a {RESULT_SCHEMA} document"
    validate_document(result, RESULT_SCHEMA, what)
    _refuse_inconsistent(result_invariants(result), what)


def validate_census(document, what: str) -> None:
    """A STORED census, against the definition its own `schema` declares.

    Both readable schemas are declared, so a v1 seed validates as a v1
    seed and is then reported as schema drift (`--validate`) or answered
    with `--seed` (every other operation) — never as corruption. The
    discriminator itself cannot be a schema keyword: which definition
    applies is the question the schema answers, not one it asks.
    """
    if not isinstance(document, dict):
        raise CensusError(
            f"{what} must be a JSON object, got {type(document).__name__}")
    declared = document.get("schema")
    if declared not in MIGRATABLE_SCHEMAS:
        raise CensusError(
            f"{what} declares schema {declared!r}, which is not one this tool "
            f"can read (expected one of {MIGRATABLE_SCHEMAS})")
    validate_document(document, declared, what)
    _refuse_inconsistent(census_invariants(document), what)


# ==========================================================================
# Cohort identity and the combined statistic (#1429)
# ==========================================================================
#
# #1428 stores measurements; this says what one cohort MEANS. The
# reader that ranks cohorts against each other, ages them and summarizes
# a record is `probe_census_summary.py`; what lives here is the identity
# and arithmetic both it and `probe_census_records.ingest_result` need.
#
# * Runs accumulate only within one commit hash. Commit identity is the
#   boundary that stops unlike code being averaged together, so pooling
#   is per-cohort and the rate is recomputed from the combined
#   numerator and denominator — never an arithmetic mean of the stored
#   per-batch rates, which would weight a two-run batch like a
#   fifty-run one.
# * Nothing here prunes, reorders or rewrites a cohort: this whole
#   section is a READER.
# * Age is measured from the cohort's own freshness anchor against an
#   evaluation time the CALLER supplies, so a test pins a clock instead
#   of racing one. Repository HEAD moving is not a census event at all —
#   only a measurement changes census state.
#
# Commit hashes have no intrinsic ordering, so "newest" is append
# order, never a comparison between two hashes: an A -> B -> A sequence
# ends with a THIRD cohort whose commit is A, not with the first one
# resurrected. `probe_census_summary.authoritative_cohort` is where that
# ordering is applied.

FULL_COMMIT_LENGTH = 40
_HEX = frozenset("0123456789abcdef")
# What `probe_flake._commit_sha` writes when `git rev-parse` could not
# be consulted. It is a well-formed result document and a legitimate
# attempt-log entry; it is not a cohort identity.
PLACEHOLDER_COMMIT = "unknown"
TIMESTAMP_FORMAT = "%Y-%m-%dT%H:%M:%SZ"
SECONDS_PER_DAY = 86400
# The default age horizon. A fortnight is long enough that a probe
# measured against a quiet subsystem is not re-measured every week, and
# short enough that a number older than it predates most of a sprint's
# merges. Policy, not physics: every entry point takes the horizon as
# an argument, and `--stale-after-days` overrides this one.
DEFAULT_STALE_AFTER_DAYS = 14
DEFAULT_STALE_AFTER_SECONDS = DEFAULT_STALE_AFTER_DAYS * SECONDS_PER_DAY

COHORT_CURRENT = "current"
COHORT_HISTORY = "history"


def require_commit_identity(value, what: str) -> str:
    """A real full Git object identity, or a controlled refusal.

    Commit identity is the whole reason cohorts exist, so the one value
    a cohort may not be keyed by is a value that does not name a
    commit. `probe_flake` writes the literal `unknown` when `git
    rev-parse` could not be consulted, and the declared schema accepts
    it because it IS a well-formed result field — pooling under it
    would silently average every unmeasurable-provenance run of every
    commit into one cohort, which is exactly what commit identity
    exists to prevent.

    Git emits lowercase hex, and the stored value is never normalised
    (the census reports the EXACT hash), so an abbreviation, an
    uppercase spelling and a placeholder are all refused rather than
    repaired.
    """
    if not isinstance(value, str):
        raise CensusError(
            f"{what} must be a commit hash string, got "
            f"{type(value).__name__}")
    if value == PLACEHOLDER_COMMIT:
        raise CensusError(
            f"{what} is the placeholder {PLACEHOLDER_COMMIT!r}, which names "
            f"no commit; a measurement whose provenance git could not "
            f"report cannot open or extend a cohort")
    if len(value) != FULL_COMMIT_LENGTH or not set(value) <= _HEX:
        raise CensusError(
            f"{what} must be {FULL_COMMIT_LENGTH} lowercase hex characters, "
            f"got {value!r}")
    return value


def parse_timestamp(value, what: str):
    """One `YYYY-MM-DDTHH:MM:SSZ` stamp as an aware UTC datetime.

    The census stores UTC and says so in the field name; this refuses
    anything it cannot read back rather than substituting a clock
    reading, because a fabricated timestamp would make a stale record
    look fresh.
    """
    if not isinstance(value, str):
        raise CensusError(
            f"{what} must be a UTC timestamp string, got "
            f"{type(value).__name__}")
    try:
        naive = datetime.datetime.strptime(value, TIMESTAMP_FORMAT)
    except ValueError:
        raise CensusError(
            f"{what} is not a `{TIMESTAMP_FORMAT}` UTC timestamp: "
            f"{value!r}") from None
    return naive.replace(tzinfo=datetime.timezone.utc)


def require_count(value, what: str) -> int:
    """A usable nonnegative aggregation count, or a controlled refusal.

    `bool` is excluded deliberately: it is an `int` in Python, and a
    `True` requested-run count would aggregate as 1 while meaning
    nothing at all.
    """
    if isinstance(value, bool) or not isinstance(value, int):
        raise CensusError(
            f"{what} must be an integer count, got {type(value).__name__}")
    if value < 0:
        raise CensusError(f"{what} must not be negative, got {value}")
    return value


def require_seconds(value, what: str) -> float:
    """A usable finite nonnegative duration, or a controlled refusal.

    `bool` is excluded for the same reason `require_count` excludes it:
    `True` is an `int`, and a one-second worst case is not what a
    boolean in that field means.
    """
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise CensusError(
            f"{what} must be a number of seconds, got {type(value).__name__}")
    if not math.isfinite(value) or value < 0:
        raise CensusError(
            f"{what} must be a finite nonnegative number of seconds, got "
            f"{value!r}")
    return float(value)


def require_measurement_semantics(result) -> None:
    """The semantic fields an ACCEPTED measurement contributes.

    The declared schema (#1492) already owns shape, type, enum, length
    and range for a result document. This is the narrower question it
    cannot answer: whether the values this issue's cohort arithmetic
    consumes actually MEAN something. It runs before a `status: "ok"`
    sample reaches a cohort, and its refusal leaves the census
    completely unmodified — no sample, and no attempt either, because
    an unusable measurement is not evidence that a run happened.

    It is deliberately not a cross-field audit (#1493): nothing here
    compares `failure_count` against `requested_runs` or a sample's
    stored rate against its own counts.
    """
    require_commit_identity(result.get("commit_sha"), "commit_sha")
    parse_timestamp(result.get("timestamp_utc"), "timestamp_utc")
    require_count(result.get("requested_runs"), "requested_runs")
    require_count(result.get("failure_count"), "failure_count")


def cohort_statistic(cohort, what: str) -> tuple[dict, "datetime.datetime"]:
    """One cohort's combined statistic, and its freshness anchor beside it.

    The anchor is the LATEST measurement timestamp contributing to the
    cohort — not the commit's own date, not the census file's mtime,
    not the moment of ingestion, and never repository HEAD. An
    out-of-order same-commit result therefore adds its counts without
    dragging the anchor backwards. It is returned separately rather
    than as a field of the statistic, because it is a `datetime` and
    everything in the statistic is JSON.

    This applies the ingestion path's semantic checks a second time, to
    ALREADY-STORED state, so a census hand-edited (or written before
    those checks existed) fails closed instead of being summarized into
    a confident wrong number. It stays the same narrow set: nothing
    here asks whether a sample's own commit matches the cohort it sits
    in, or whether its stored rate agrees with its counts — those are
    #1493's cross-field invariants.
    """
    if not isinstance(cohort, dict):
        raise CensusError(
            f"{what} must be an object, got {type(cohort).__name__}")
    commit = require_commit_identity(cohort.get("commit_sha"),
                                     f"{what} `commit_sha`")
    samples = cohort.get("samples")
    if not isinstance(samples, list):
        raise CensusError(f"{what} `samples` must be a list")
    if not samples:
        # A cohort exists because a measurement created it, so an empty
        # one has no anchor and no denominator. Refusing says so;
        # falling through to an older cohort would invent a statistic
        # for a commit nobody measured.
        raise CensusError(
            f"{what} holds no samples, so it has no measurement to "
            f"summarize")
    requested = 0
    failures = 0
    latest = None
    for position, sample in enumerate(samples):
        where = f"{what} sample {position}"
        if not isinstance(sample, dict):
            raise CensusError(
                f"{where} is not an object, got {type(sample).__name__}")
        require_commit_identity(sample.get("commit_sha"),
                                f"{where} `commit_sha`")
        stamp = parse_timestamp(sample.get("timestamp_utc"),
                                f"{where} `timestamp_utc`")
        requested += require_count(sample.get("requested_runs"),
                                   f"{where} `requested_runs`")
        failures += require_count(sample.get("failure_count"),
                                  f"{where} `failure_count`")
        if latest is None or stamp > latest:
            latest = stamp
    return {
        "commit_sha": commit,
        "sample_count": len(samples),
        "requested_runs": requested,
        "failure_count": failures,
        # Recomputed from the COMBINED numerator and denominator. A
        # cohort whose samples all requested zero runs has no rate at
        # all, which is not the same observation as a rate of zero.
        "failure_rate": None if requested == 0 else failures / requested,
        "measured_at": latest.strftime(TIMESTAMP_FORMAT),
    }, latest
