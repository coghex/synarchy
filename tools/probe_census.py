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

* `acceptable_failures` — X, the acceptable-failure count, with the
  justification a nonzero one must carry. #1430 chose the policy #1428
  staged: X is the number of failures a COMPLETE ten-run measurement may
  show and still be acceptable, it is an integer from 0 through 9, X=0
  is the default every probe starts at, and X above 0 is a maintainer's
  written decision that a CI-eligible probe may not hold at all. See
  "The acceptable-failure policy" below.
* `estimated_worst_case_seconds` — supplied metadata, deliberately
  distinct from the OBSERVED `worst_elapsed_seconds` of a sample.
* `current` — the current commit cohort: the cohort of the most recently
  accepted measurement commit. Another measurement for the SAME commit
  appends to it; a different commit archives the whole prior cohort into
  `history` first. No cohort or sample is ever overwritten or discarded.
* `history` — archived cohorts, append-only, retained forever. A probe
  promoted to CI eligibility keeps its history; it just stops receiving
  current samples. #1431 made that last clause a storage invariant
  rather than a convention: `--record` refuses a live CI-eligible probe
  outright (`refuse_ci_eligible_measurement`), so a result document
  produced before the promotion cannot reopen a cohort afterwards.
* `attempts` — an append-only log of well-formed ingestion attempts. A
  well-formed harness-error result is logged but contributes no sample
  and no aggregate.
* `claims` — #1434 extends the file again, to `probe-census/v3`, with an
  append-only log of successful per-probe claim ACQUISITIONS, written by
  `tools/probe_claim.py` before the measurement each one authorizes
  runs. It is deliberately a separate collection rather than more
  `attempts`: an attempt is a result ingestion and is non-idempotent by
  design, while an acquisition is identified by its acquisition TOKEN
  and stays one record however many times the recorder retries. The
  v2→v3 migration adds the empty log and nothing else.
* `outcomes` — #1439 extends the file again, to `probe-census/v4`, with
  an append-only log of a de-flake attempt's endings that produced no
  verified repair, written by `tools/deflake_outcome.py` for the three
  STABLE NON-SUCCESS outcomes and by `tools/deflake_issue.py` (#1438)
  for the production defect it files a tracker issue for. A third
  separate collection for the same reason the second one was: it is
  identified by its ATTEMPT identity and stays one record however many
  times the workflow is resumed, and it carries evidence — the
  measurement summaries the outcome rests on, the diagnostic summary,
  an advisory de-list recommendation, and the filed issue's number, URL
  and publication key — that neither of the other two logs has a field
  for. The v3→v4 migration adds the empty log and nothing else.
* `deferred` — v5's nullable maintainer decision. When present it states
  both why the probe cannot produce meaningful evidence yet and the
  condition that makes it ready to resume. The selector excludes the
  probe before claiming or running it; all existing policy, measurements,
  claims and outcomes remain intact. The v4→v5 migration adds null and
  nothing else.

#1429 adds what those measurements MEAN over time. The newest cohort is
the current statistic and displaces the previous one without deleting
it; runs accumulate only within one commit hash, and a cohort's rate is
recomputed from the combined numerator and denominator rather than
averaged across batches of unequal size; every cohort is retained for
the lifetime view; and staleness is purely age-based, measured from the
cohort's own latest measurement timestamp against an evaluation time
the CALLER supplies. Commits never invalidate a record and repository
HEAD moving is not a census event — only a measurement changes census
state. `--summary` is the selection-facing view: it distinguishes an
unmeasured probe from a measured one, and reports the authoritative
cohort's exact commit, its latest measurement, its nonnegative age, its
stale flag and its combined run/failure counts and rate.

Only summarized outcomes and external artifact references live here.
Raw stdout, protocol event streams and engine logs stay in the
harness's artifact tree, outside every worktree.

SHAPE VALIDATION IS DECLARED, NOT HAND-ROLLED (#1492). Every document
this module reads or writes is checked against
`tools/probe_census_schema.json`, a JSON Schema 2020-12 document that is
itself self-checked against that draft on load: the stored census before
any operation transforms it, the incoming `probe-flake-result/v1`
document before one nested field of it is read, and the complete
candidate census immediately before the atomic replacement installs it.
The schema owns presence, closure, primitive type, enum, length and
representable range; `_reject_non_finite` covers the one thing JSON
Schema's numeric keywords cannot (`json.loads` accepts `NaN` and
`Infinity`, and no comparison with a NaN is ever true). A violation is a
controlled refusal naming the offending JSON path — never a traceback,
never a partial write — and an absent `jsonschema` is ONE loud error
carrying the install command, never a silently skipped check.

CROSS-FIELD INVARIANTS ARE CODE, NOT SCHEMA (#1493). The rules that span
fields cannot be declared, so `census_invariants` and `result_invariants`
state them directly: accepted attempts reconcile against retained
samples; `accepted` agrees with `status`; a harness error never reports
completing every requested run; `check_counts` is keyed by exactly the
descriptor's checks and each entry is the tally `runs` shows; a PASS run
carries no FAIL check; a cohort holds one commit's samples; and a deferral
has both non-blank human-facing fields. Each rejects state no real run
could have written, and each runs on both sides of a mutation exactly as
the schema does. They are distinct from #1429's SEMANTIC checks, which
stay narrow by design: only the commit identity, timestamp and counts the
cohort arithmetic itself consumes.

THE ACCEPTABLE-FAILURE POLICY IS CODE TOO (#1430), and asymmetric. The
schema bounds X to 0..9 while still admitting the null a pre-policy
census holds; `policy_invariants` closes that null, requires a
non-whitespace justification above 0, and refuses tolerance on a
CI-eligible probe — on every mutation's CANDIDATE and on `--validate`,
but never on the stored side, because `--seed` has to be able to READ a
null X in order to initialize it to 0. That single initialization is
the only automatic policy repair there is: a malformed stored X stays
visible rather than being silently corrected. `tolerance_state` applies
the threshold, and only to ONE complete ten-run measurement, which
`policy_sample` picks out of a cohort — a cohort's pooled totals are the
basis for its RATE, never for a fixed-N threshold.

What is deliberately still absent: any requirement that the census agree
with the live probe registry, which stays `validate_manifest`'s report
and `--seed`'s repair. This is not an exhaustive corruption detector
either — a hand-edited census is not an adversarial input class, and
malformed state must fail SAFELY rather than exhaustively.

What this file owns is MUTATION: the schema and its migrations, the
validators, the reconciliation against the live registry, the lock, the
atomic write, the docs-worktree resolution and every policy update.
#1441's CI-promotion assessment and report rendering are read-only over
a census this file has already loaded and validated, and since #2034
they live in `tools/probe_census_promotion.py` — imported at its point
of use in `--promotion-candidates`' dispatch so the one-way dependency
(promotion -> census) closes no cycle. That CLI mode's arguments,
incompatibilities, exit codes, human rendering and `--json` structure
are unchanged by the move; `promotion_report` and
`render_promotion_report` are simply no longer attributes of this
module.

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
  # A stored probe-census/v1 through /v4 census migrates in place here,
  # losing no policy field, cohort, sample, attempt, claim or outcome;
  # `--record` and the policy operations refuse an unmigrated census by
  # name.
  python3 tools/probe_census.py --validate         # check the docs-wip copy
  python3 tools/probe_census.py --record RESULT    # ingest one measurement
  python3 tools/probe_census.py --summary          # the current statistics
  python3 tools/probe_census.py --summary --probe KEY --json
  # Age is measured against an evaluation time and an age horizon, both
  # supplied rather than assumed, so a report is reproducible.
  python3 tools/probe_census.py --summary --as-of 2026-08-21T05:00:00Z \
      --stale-after-days 7
  python3 tools/probe_census.py --promotion-candidates  # who could be promoted
  # Reliability only, and it edits nothing: two lists, one of probes
  # whose every manual-only ground a measurement can answer, one of
  # equally clean probes held out on a ground it never could. Breadth,
  # cost, runner support and the promotion itself are a person's.
  python3 tools/probe_census.py --promotion-candidates --json \
      --as-of 2026-08-21T05:00:00Z --stale-after-days 7
  python3 tools/probe_census.py --probe KEY --set-acceptable-failures 2 \
      --justification "two known engine-side races"
  # X only. Omitting --justification NEVER clears the stored text, and
  # an X above 0 needs one already stored or supplied here.
  python3 tools/probe_census.py --probe KEY --set-acceptable-failures 7
  # The only way to clear it: never combined with --justification, and
  # only while setting X back to 0.
  python3 tools/probe_census.py --probe KEY --set-acceptable-failures 0 \
      --clear-justification
  python3 tools/probe_census.py --probe KEY --set-estimate 480
  python3 tools/probe_census.py --defer --probe KEY \
      --reason "the required content is not implemented" \
      --resume-when "the planned content assets merge"
  python3 tools/probe_census.py --resume --probe KEY
"""
from __future__ import annotations

import argparse
import contextlib
import datetime
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
import probe_protocol  # noqa: E402
import probe_engine  # noqa: E402
import probe_runner_registry  # noqa: E402

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

MANIFEST_RELPATH = "docs/probe_census.json"
DOCS_BRANCH = "docs-wip"

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


class CensusDurabilityUnconfirmed(Exception):
    """The replacement is ALREADY VISIBLE, and making it durable failed.

    Deliberately NOT a `CensusError`. That type's whole contract is the
    one `update` states — a failure before `os.replace` leaves the old
    authoritative bytes untouched — and by the time this is raised the
    new complete census is what every later reader will parse. Reporting
    it as an ordinary refusal would tell a caller its measurement was
    not recorded when it may well have been, and census ingestion is
    append-only and deliberately non-idempotent, so a caller that
    believed that and retried would duplicate the sample.

    It exists so a caller can distinguish the two sides DETERMINISTICALLY
    (#1436): the staging write and the pre-replacement fsync raise
    `OSError` from inside `_atomic_replace`'s try/except, the
    post-replacement directory fsync sits outside it, and both used to
    reach the caller as an indistinguishable bare `OSError`. Classifying
    them by exception message would be guesswork; this is the signal.

    `error` is the underlying failure and `target` the census that was
    replaced, so a report can name both.
    """

    def __init__(self, message: str, *, target, error):
        super().__init__(message)
        self.target = Path(target)
        self.error = error


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


# ==========================================================================
# Cohort semantics: the current statistic, its age, and staleness (#1429)
# ==========================================================================
#
# #1428 stores measurements; this says what they MEAN over time.
#
# * The newest cohort wins. The cohort created by the latest accepted
#   measurement is the current statistic; it DISPLACES the previous one
#   as the current number without deleting it.
# * Runs accumulate only within one commit hash. Commit identity is the
#   boundary that stops unlike code being averaged together, so pooling
#   is per-cohort and the rate is recomputed from the combined
#   numerator and denominator — never an arithmetic mean of the stored
#   per-batch rates, which would weight a two-run batch like a
#   fifty-run one.
# * History is retained for the lifetime view. Nothing here prunes,
#   reorders or rewrites a cohort: this whole section is a READER.
# * Staleness is purely age-based. A commit never invalidates a record,
#   and repository HEAD moving is not a census event at all — only a
#   measurement changes census state. Age is measured from the cohort's
#   own freshness anchor against an evaluation time the CALLER
#   supplies, so a test pins a clock instead of racing one.
#
# Commit hashes have no intrinsic ordering, so "newest" is append
# order, never a comparison between two hashes: an A -> B -> A sequence
# ends with a THIRD cohort whose commit is A, not with the first one
# resurrected.
#
# The authoritative cohort is `current` when the record carries one and
# the final `history` entry otherwise. That second case is real rather
# than defensive: `reconcile_inventory` archives `current` when a probe
# is promoted to CI eligibility and deliberately does not restore it on
# a later downgrade, so a promoted probe's newest measured statistic
# lives in `history[-1]`. Only a record with neither is UNMEASURED, and
# an unmeasured probe reports null measurements — never a zero failure
# rate, which is a real and very different observation.
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


def authoritative_cohort(census, probe: str):
    """The newest retained cohort and where it lives, or None.

    `current` when the record carries one; otherwise the final
    append-ordered `history` entry, which is where a probe promoted to
    CI eligibility keeps its newest measured statistic. None means
    UNMEASURED — neither a current cohort nor a single archived one.
    """
    if not isinstance(census, dict):
        raise CensusError(
            f"probe {probe!r} has no census record to summarize")
    current = census.get("current")
    if current is not None:
        return current, COHORT_CURRENT
    history = census.get("history")
    if history is None:
        history = []
    if not isinstance(history, list):
        raise CensusError(
            f"probe {probe!r}: `history` must be a list, got "
            f"{type(history).__name__}")
    if not history:
        return None
    return history[-1], COHORT_HISTORY


def require_horizon(stale_after_seconds):
    """A usable nonnegative age horizon, or a controlled refusal."""
    if isinstance(stale_after_seconds, bool) or not isinstance(
            stale_after_seconds, (int, float)):
        raise CensusError(
            f"the staleness horizon must be a number of seconds, got "
            f"{type(stale_after_seconds).__name__}")
    if not math.isfinite(stale_after_seconds) or stale_after_seconds < 0:
        raise CensusError(
            f"the staleness horizon must be a finite nonnegative number of "
            f"seconds, got {stale_after_seconds!r}")
    return stale_after_seconds


def summarize_entry(entry, *, now, stale_after_seconds) -> dict:
    """The selection-facing view of ONE census row.

    `measured` is the field that separates "this probe has never been
    measured" from "this probe was measured and never failed"; every
    measurement field of an unmeasured probe is null, so a zero rate
    can only ever mean an observed zero. `stale` is a property of a
    cohort, so it is null — not True — when there is no cohort: absent
    data is a different selection input from old data.

    `age_seconds` is clamped at zero. A cohort anchored in the future
    (a skewed clock, an injected evaluation time) is the freshest thing
    there is, never negative, and a negative age would sort ahead of
    every real measurement.

    `tolerance` reports the record's own X against ONE measurement
    (#1430) — the authoritative cohort's last-appended complete
    `POLICY_RUN_COUNT`-run sample, never its pooled totals, which are
    the right basis for `failure_rate` and the wrong one for a
    fixed-N threshold. It is `not-comparable` when the record has no
    usable X or that cohort holds no such measurement, an unmeasured
    probe included.
    """
    if not isinstance(entry, dict):
        raise CensusError(
            f"census entry must be an object, got {type(entry).__name__}")
    probe = entry.get("key")
    if not isinstance(probe, str):
        raise CensusError(
            f"census entry has no string `key` ({entry.get('key')!r})")
    horizon = require_horizon(stale_after_seconds)
    census = entry.get("census")
    acceptable = (census.get("acceptable_failures")
                  if isinstance(census, dict) else None)
    deferred = require_deferral(
        census.get("deferred") if isinstance(census, dict) else None,
        f"probe {probe!r} `deferred`")
    summary = {
        "key": probe,
        "script": entry.get("script"),
        "classification": entry.get("classification"),
        "acceptable_failures": acceptable,
        "deferred": deferred,
        # A record with no measurement is compared against nothing, so
        # the policy neither passes nor breaches: it does not apply yet.
        "tolerance": TOLERANCE_NOT_COMPARABLE,
        "measured": False,
        "cohort": None,
        "commit_sha": None,
        "measured_at": None,
        "age_seconds": None,
        "stale": None,
        "sample_count": None,
        "requested_runs": None,
        "failure_count": None,
        "failure_rate": None,
    }
    located = authoritative_cohort(census, probe)
    if located is None:
        return summary
    cohort, source = located
    statistic, anchor = cohort_statistic(
        cohort, f"probe {probe!r} {source} cohort")
    age = max(0.0, (now - anchor).total_seconds())
    summary.update(statistic)
    summary["measured"] = True
    summary["cohort"] = source
    summary["age_seconds"] = age
    summary["stale"] = age >= horizon
    # ONE measurement, never the cohort's pooled totals: X is stated
    # against a complete ten-run run, so `statistic`'s combined counts
    # are the wrong basis for it even though they are the right one for
    # the rate beside it.
    measurement = policy_sample(cohort)
    if measurement is not None:
        summary["tolerance"] = tolerance_state(
            acceptable, measurement["requested_runs"],
            measurement["completed_runs"], measurement["failure_count"])
    return summary


def census_summary(document, *, now, stale_after_seconds,
                   probe: str | None = None) -> list[dict]:
    """The selection-facing view of the whole census, or of one probe.

    `now` is supplied by the caller and never read from the wall clock
    here: staleness is a function of an evaluation time, and a test
    that injects one is testing the classification rather than racing
    it.
    """
    if not isinstance(now, datetime.datetime) or now.tzinfo is None:
        raise CensusError(
            "the evaluation time must be a timezone-aware datetime")
    horizon = require_horizon(stale_after_seconds)
    entries = _rows(document, "census")
    if probe is not None:
        entries = [target_row(document, probe, "--summary")]
    return [summarize_entry(entry, now=now, stale_after_seconds=horizon)
            for entry in entries]


# ==========================================================================
# The docs worktree
# ==========================================================================
CREATE_DOCS_WORKTREE = (
    f"  git worktree add ~/work/synarchy-docs -b {DOCS_BRANCH} origin/master")


def _worktree_records(stdout: str) -> list[dict]:
    """`git worktree list --porcelain` as one dict per blank-line record.

    Attributes are parsed whole rather than line-matched, because
    `prunable` is an attribute of the record it follows: a registered
    worktree whose directory is gone still prints its `worktree` and
    `branch` lines, and only the trailing `prunable <reason>` says it is
    no longer usable.
    """
    records: list[dict] = []
    current: dict = {}
    for line in stdout.splitlines():
        if not line.strip():
            if current:
                records.append(current)
            current = {}
            continue
        name, _, value = line.partition(" ")
        current[name] = value.strip()
    if current:
        records.append(current)
    return records


def resolve_docs_worktree(repo_root: str | None = None) -> Path:
    """The worktree whose branch is `docs-wip`, resolved BY BRANCH.

    The same idiom `tools/docs_land.sh` uses. A missing docs worktree is
    an actionable stop, never a silent fall back to the primary checkout
    (which the PR drainer must be able to fast-forward) and never an
    implicit `git worktree add` performed as a side effect.

    A REGISTERED-BUT-UNUSABLE worktree is the same stop. Git keeps
    listing a worktree whose directory has been deleted, marking the
    record `prunable`; returning that path anyway would let the writer
    recreate the directory and publish the census outside any worktree
    at all — silently, in a place nobody will ever land from.
    """
    root = repo_root or probe_engine.REPO_ROOT
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
    for record in _worktree_records(done.stdout):
        if record.get("branch") != f"refs/heads/{DOCS_BRANCH}":
            continue
        path = Path(record.get("worktree", ""))
        if "prunable" in record:
            raise DocsWorktreeMissing(
                f"the worktree registered for {DOCS_BRANCH} at {path} is "
                f"prunable ({record['prunable'] or 'unusable'}). Clear the "
                f"stale registration and recreate it with:\n"
                f"  git worktree prune\n{CREATE_DOCS_WORKTREE}")
        if not path.is_dir() or not (path / ".git").exists():
            raise DocsWorktreeMissing(
                f"the worktree registered for {DOCS_BRANCH} at {path} is not "
                f"a usable checkout. Clear the stale registration and "
                f"recreate it with:\n"
                f"  git worktree prune\n{CREATE_DOCS_WORKTREE}")
        return path
    raise DocsWorktreeMissing(
        f"no worktree is on branch {DOCS_BRANCH}. Create one with:\n"
        f"{CREATE_DOCS_WORKTREE}")


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


def _atomic_replace(target: Path, payload: bytes, *, what: str = "census",
                    prefix: str = STAGING_PREFIX) -> None:
    """Install `payload` as `target` in one step.

    The staging file is a SIBLING so the rename never crosses a
    filesystem, the bytes are fsynced before the rename so a crash
    cannot promote a short file, and the directory is fsynced after so
    the rename itself is durable.

    `what` and `prefix` name the artifact for a caller that is not the
    census itself — `tools/probe_census_page.py` writes the generated
    page into the same directory, and needs a staging prefix
    `_clear_staging` does NOT sweep, since that sweep runs under the
    census lock and would otherwise unlink a live page staging file.
    """
    target.parent.mkdir(parents=True, exist_ok=True)
    fd, staged = tempfile.mkstemp(dir=str(target.parent),
                                  prefix=prefix, suffix=STAGING_SUFFIX)
    staged_path = Path(staged)
    try:
        # `mkstemp` creates with O_EXCL, so the staging path cannot be a
        # pre-planted symlink; assert the resulting inode anyway, since
        # this is the third path the substitution rule names.
        info = os.fstat(fd)
        if not stat.S_ISREG(info.st_mode) or info.st_nlink != 1:
            raise CensusError(
                f"refusing to use {staged_path}: the {what} staging file must "
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
    # Everything above either succeeded or left the old bytes in place.
    # From here the replacement is VISIBLE, so a failure is a durability
    # question and not a "did it happen" question — and the two have to
    # be told apart by their type rather than by their message, because
    # a caller's recovery differs completely (#1436). Nothing is retried
    # or rolled back here: the rename has landed and undoing it would
    # discard a committed append-only update.
    try:
        dir_fd = os.open(str(target.parent), os.O_RDONLY)
    except OSError as error:
        raise CensusDurabilityUnconfirmed(
            f"the {what} at {target} was replaced, but its directory could "
            f"not be opened to make the rename durable ({error}); the new "
            f"content is already visible", target=target, error=error) from None
    try:
        os.fsync(dir_fd)
    except OSError as error:
        raise CensusDurabilityUnconfirmed(
            f"the {what} at {target} was replaced, but the directory fsync "
            f"that makes the rename durable failed ({error}); the new "
            f"content is already visible", target=target, error=error) from None
    finally:
        with contextlib.suppress(OSError):
            os.close(dir_fd)


# ==========================================================================
# The preservation contract
# ==========================================================================
def _entry_map(document) -> dict:
    return {entry["key"]: entry for entry in (document or {}).get("probes") or []
            if isinstance(entry, dict) and isinstance(entry.get("key"), str)}


def _sample_total(census) -> int:
    """Every retained sample a census record holds, current and archived.

    Counts only what is countable and never raises. The declared schema
    now refuses a cohort whose `samples` is not a list before this ever
    sees one, so the tolerance is no longer load-bearing for STORED
    state — but it is kept, because this also runs over the CANDIDATE,
    which the schema has not yet checked at that point, and a list that
    BECOMES uncountable still reads as a drop from its old length.
    """
    if not isinstance(census, dict):
        return 0
    history = census.get("history")
    cohorts = list(history) if isinstance(history, list) else []
    if census.get("current") is not None:
        cohorts.append(census["current"])
    total = 0
    for cohort in cohorts:
        if not isinstance(cohort, dict):
            continue
        samples = cohort.get("samples")
        if isinstance(samples, list):
            total += len(samples)
    return total


POLICY_FIELDS = ("acceptable_failures", "acceptable_failures_justification",
                 "estimated_worst_case_seconds")
MEASUREMENT_FIELDS = ("current", "history", "attempts")
# #1434's claim log is its own aspect, not a measurement field: a claim
# is recorded BEFORE the measurement runs and may not carry a cohort, a
# sample or an attempt with it, and a measurement may not append a
# claim. Keeping the two sets disjoint is what makes each operation's
# `touched` aspect mean exactly one thing.
CLAIM_FIELDS = ("claims",)
# #1439's de-flake outcome log, a fourth aspect for the same reason the
# claim log was a third: an outcome is appended AFTER a diagnosis, is
# idempotent on its attempt identity, and may not create a cohort, a
# sample, an attempt or a claim on its way in.
OUTCOME_FIELDS = ("outcomes",)
# A maintainer-controlled availability gate. It neither rewrites policy
# nor discards evidence; it only tells the selector to pause this row until
# the recorded resume condition is satisfied.
DEFERRAL_FIELDS = ("deferred",)

# Each mutating aspect, the record fields it exclusively owns, and the
# operation a reader should be told about when a candidate touched a
# field it does not own. One table rather than four hand-written blocks,
# so a fifth aspect is a row here instead of another pair of loops that
# can be forgotten.
ASPECT_FIELDS = {
    "policy": POLICY_FIELDS,
    "measurements": MEASUREMENT_FIELDS,
    "claims": CLAIM_FIELDS,
    "outcomes": OUTCOME_FIELDS,
    "deferral": DEFERRAL_FIELDS,
}
ASPECT_LABEL = {
    "policy": "a policy update",
    "measurements": "a measurement ingestion",
    "claims": "a claim acquisition",
    "outcomes": "a diagnosis outcome",
    "deferral": "a deferral update",
}
# The aspects whose append-only logs `_append_only` compares. A policy
# update appends nothing, so it is not one of them.
APPENDING_ASPECTS = ("measurements", "claims", "outcomes")

INVENTORY_FIELDS = ("key", "script", "classification", "protocol")


def _is_initialized_x(was, now) -> bool:
    """`was`/`now` is exactly the unset-X-to-default transition (#1430).

    `_is_x` rather than a bare `now == DEFAULT_ACCEPTABLE_FAILURES`
    because `False == 0`, and a candidate that turned an unset X into a
    boolean has not initialized anything.
    """
    return (was is None and _is_x(now)
            and now == DEFAULT_ACCEPTABLE_FAILURES)


def _census_of(entry) -> dict:
    census = (entry or {}).get("census")
    return census if isinstance(census, dict) else {}


def _append_only(key: str, was: dict, now: dict) -> list[str]:
    """`history`, `attempts`, `claims` and `outcomes` grew by appending."""
    problems: list[str] = []
    for field in ("history", "attempts", "claims", "outcomes"):
        previous = was.get(field)
        current = now.get(field)
        previous = [] if previous is None else previous
        current = [] if current is None else current
        # Reported rather than sliced blindly: a stored `history: 5` is a
        # field this comparison cannot be made against, and slicing it
        # would raise from inside the preservation check.
        if not isinstance(previous, list) or not isinstance(current, list):
            problems.append(
                f"probe {key!r} `{field}` must be a list to compare "
                f"append-only, got {type(previous).__name__} before and "
                f"{type(current).__name__} after")
            continue
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
    knows nothing about field shapes — this is the preservation
    contract, a different question from the declared schema validation
    that brackets it.

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
            # never changes a deferral or loses a measurement.
            problems += _append_only(key, was, now)
            for field in POLICY_FIELDS:
                if now.get(field) == was.get(field):
                    continue
                if field == "acceptable_failures" and _is_initialized_x(
                        was.get(field), now.get(field)):
                    # The ONE policy transition reconciliation may make
                    # (#1430): an unset X becomes the default. Stated
                    # here rather than assumed, so a candidate that
                    # moves any other policy value — or moves X from
                    # one number to another — is still refused.
                    continue
                problems.append(
                    f"probe {key!r}: reconciliation changed policy field "
                    f"`{field}`")
            for field in DEFERRAL_FIELDS:
                if now.get(field) != was.get(field):
                    problems.append(
                        f"probe {key!r}: reconciliation changed deferral "
                        f"field `{field}`")
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
        if allowed & set(APPENDING_ASPECTS):
            # One append-only comparison covers `history`, `attempts`,
            # `claims` and `outcomes` together; the per-aspect equality
            # checks below are what keep an operation inside the aspect
            # it declared.
            problems += _append_only(key, was, now)
        # Names the OPERATION, not just the field: "a policy update may
        # not touch `attempts`" is the sentence a reader needs. The
        # operation is whichever aspect the mutation declared, so the
        # wording stays true as aspects are added rather than being a
        # chain of two-way guesses.
        label = next((ASPECT_LABEL[aspect] for aspect in ASPECT_LABEL
                      if aspect in allowed), "this operation")
        for aspect, fields in ASPECT_FIELDS.items():
            if aspect in allowed or aspect == "policy":
                # `policy` is reported by the dedicated loop above,
                # which names the offending field rather than the
                # operation, and is the wording #1430's tests pin.
                continue
            for field in fields:
                if now.get(field) != was.get(field):
                    problems.append(
                        f"probe {key!r}: the candidate changed `{field}`, "
                        f"which {label} may not touch")
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

    Declared schema validation (#1492) brackets the mutation: the stored
    document is checked BEFORE `mutate` transforms it, and the complete
    candidate is checked immediately before the replacement — against
    the serialized bytes, so what is validated is exactly what a later
    reader will parse.

    Any failure before `os.replace` — a schema violation on either side,
    a refusing mutation, an unserializable candidate, a preservation
    violation, a staging write that dies — leaves the old authoritative
    bytes untouched.

    AFTER the replacement there is exactly one thing left to fail, and
    it raises `CensusDurabilityUnconfirmed` rather than the `CensusError`
    that promise belongs to: the directory fsync that makes the rename
    durable. The new census is already what a later reader parses by
    then, so a caller must not treat it as a refusal and must not retry
    an append-only ingestion against it.
    """
    path = Path(path)
    with _locked(path):
        before = read_for_update(path)
        if before is not None:
            validate_census(before, f"census {path}")
        try:
            candidate, touched = mutate(before)
            payload = render_manifest(
                candidate, f"the candidate census for {path}").encode("utf-8")
            # Compare against the bytes a later reader will see, not the
            # in-memory object that produced them.
            installed = json.loads(payload.decode("utf-8"))
            problems = _check_preserved(before, installed, touched)
        except CensusError:
            raise
        except (TypeError, ValueError, KeyError, AttributeError,
                IndexError) as error:
            # The safety boundary the issue requires, at the ONE funnel
            # every mutation passes through, and covering the WHOLE
            # candidate derivation — the mutation, the serialization and
            # the preservation comparison alike, since malformed stored
            # state reaches all three. A structural or type error met
            # while performing the operation becomes a controlled
            # refusal instead of a traceback. It is not schema
            # validation — the declared schema owns shape on both
            # sides of this, and #1493 owns the cross-field invariants;
            # this reports only what actually blocked the operation.
            raise CensusError(
                f"census {path} is structurally malformed for this operation "
                f"({type(error).__name__}: {error})") from None
        if problems:
            raise CensusError("refusing to install this candidate census: " +
                              "; ".join(problems[:5]))
        # The last gate before the replacement. It runs AFTER the
        # preservation comparison so a candidate that both loses data
        # and violates the schema reports the loss, which is the more
        # actionable of the two, and it validates the SERIALIZED bytes
        # rather than the object that produced them. A candidate is
        # always the CURRENT schema whatever the stored document was, so
        # the definition is named rather than rediscovered from the
        # candidate's own field.
        validate_document(installed, CENSUS_SCHEMA,
                          f"the candidate census for {path}")
        # The cross-field invariants (#1493) on the same two sides the
        # schema is applied to. The stored check above is what refuses
        # to rewrite an inconsistent census; this one is what refuses to
        # CREATE one, so no mutation can install state the next read
        # would reject.
        _refuse_inconsistent(census_invariants(installed),
                             f"the candidate census for {path}")
        # The acceptable-failure policy (#1430), on the CANDIDATE only.
        # Deliberately not bracketed like the two checks above: a census
        # written before this policy existed holds null Xs, and `--seed`
        # must be able to read one in order to initialize them. What no
        # operation may do is install a policy-invalid census.
        _refuse_policy(policy_invariants(installed),
                       f"the candidate census for {path}")
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


def refuse_ci_eligible_measurement(probe: str) -> None:
    """A CI-eligible probe takes no census sample (#1431).

    "A promoted probe receives no further census samples" is a STORAGE
    invariant, not only a reporting one. `probe_flake.resolve_probe`
    already refuses to RUN a CI-eligible probe, but a result document
    outlives the run that produced it: one measured before a promotion,
    or replayed from an artifact tree afterwards, would otherwise be
    ingested into a row whose classification says it is CI's now.
    Eligibility is read LIVE from `tools/ci_probes.py`, the same
    authority `classification` reads, never from the stored row — a
    census not yet reconciled by `--seed` still holds the old label.

    The refusal covers a harness error too. It is not a judgement about
    the sample: nothing about a promoted probe belongs in the census's
    append-only record, and its retained history stays exactly as the
    promotion left it.
    """
    if probe in ci_probes.CI_ELIGIBLE:
        raise CensusError(
            f"probe {probe!r} is CI-eligible, so the census accepts no "
            f"measurement for it: CI runs it on every matching PR, and a "
            f"promoted probe keeps its manifest row and its retained history "
            f"while receiving no further samples. tools/ci_probes.py is the "
            f"authority on that classification.")


def record_result_installed(path: Path, result) -> tuple:
    """Ingest one result and return `(probe, installed_census)`.

    `update` already returns the candidate it installed;
    `record_result` discards it and answers only the probe key. A caller
    that needs a field of the row it just wrote — #1659's handoff needs
    the acceptable-failure count — must read it from THAT document and
    not from a later reread: the lock is released when `update` returns,
    so a second read can answer with another agent's edit and attribute
    it to this measurement.
    """
    validate_result(result)
    refuse_ci_eligible_measurement(result["probe"])
    touched: list[str] = []

    def mutate(before):
        document = require_current_schema(before, path)
        candidate, probe = ingest_result(document, result)
        touched.append(probe)
        return candidate, {probe: {"measurements"}}
    installed = update(path, mutate)
    return touched[0], installed


def record_result(path: Path, result) -> str:
    """Ingest one `probe-flake-result/v1` document. Returns the probe.

    The result is validated against its declared schema HERE — before
    the census is locked, and so before one nested ingestion field is
    read. That ordering is what makes `runs[i].checks` safe to reach at
    all: a truthy non-object there used to raise from inside the
    transaction rather than refuse in front of it.

    Live CI eligibility is refused in the same place and for the same
    reason: in front of the lock, so a promoted probe's census bytes
    are never even opened for a measurement it may not receive.
    """
    return record_result_installed(path, result)[0]


def record_claim(path: Path, probe: str, claim) -> str:
    """Durably record one successful claim acquisition. Returns the probe.

    The same locked read-modify-write every other mutation uses, so a
    recorder contending with a measurement ingestion or a policy edit
    serializes against it rather than losing an update. A replay of an
    already-recorded token installs the identical bytes, which `update`
    recognizes as a no-op and leaves the file, its inode and its mtime
    alone.
    """
    def mutate(before):
        document = require_current_schema(before, path)
        candidate, key = ingest_claim(document, probe, claim)
        return candidate, {key: {"claims"}}
    update(path, mutate)
    return probe


def record_outcome_installed(path: Path, probe: str, outcome, *,
                             reconcile=None) -> tuple:
    """Record one outcome and return `(probe, resumed, installed)`.

    `installed` is the record as it now sits in the census — which is
    the reconciled one when `reconcile` ran, so a caller reports what
    the census holds rather than the candidate it proposed.

    `resumed` is decided INSIDE the transaction, while the lock is held
    — it is true exactly when the stored census already carried this
    attempt identity. A caller that instead compared the file's bytes
    before and after would be reading them outside the lock, and a
    concurrent writer's unrelated edit would make a genuine first append
    look like a resume.

    `reconcile(candidate, stored)` is the same window offered to the
    caller. Idempotency is the WHOLE record, so a caller whose record
    carries a field it cannot derive — a wall-clock stamp is the one
    that exists — needs the STORED record to reproduce itself, and the
    only race-free place to read it is here. It runs only when this
    attempt is already recorded, and whatever it returns is what
    `ingest_outcome` then holds to that stored record: it can make a
    replay identical, and it cannot make two genuinely different
    outcomes agree.
    """
    seen: list[tuple] = []

    def mutate(before):
        document = require_current_schema(before, path)
        candidate = outcome
        attempt = (candidate.get("attempt")
                   if isinstance(candidate, dict) else None)
        stored = None
        if isinstance(attempt, str) and attempt:
            row = find_entry(document, probe)
            stored = find_outcome((row or {}).get("census"), attempt)
        if stored is not None and reconcile is not None:
            candidate = reconcile(candidate, stored)
        seen.append((stored is not None, candidate))
        installed, key = ingest_outcome(document, probe, candidate)
        return installed, {key: {"outcomes"}}
    update(path, mutate)
    resumed, record = seen[-1]
    return probe, resumed, _deep_copy(record)


def record_outcome(path: Path, probe: str, outcome) -> str:
    """Durably record one stable de-flake outcome. Returns the probe.

    The same locked read-modify-write every other mutation uses, so an
    outcome append contending with a measurement ingestion, a claim
    acquisition or a policy edit serializes against it rather than
    losing an update. Read, validation, append, serialization validation
    and the atomic replacement all happen under the one lock `update`
    holds. A resume of an already-recorded attempt installs the
    identical bytes, which `update` recognizes as a no-op and leaves the
    file, its inode and its mtime alone.

    There is deliberately no second state store and no second write
    path: #1428's writer is the one that owns this file.
    """
    return record_outcome_installed(path, probe, outcome)[0]


def record_policy(path: Path, probe: str, **fields) -> str:
    def mutate(before):
        document = require_current_schema(before, path)
        candidate, key = set_policy(document, probe, **fields)
        return candidate, {key: {"policy"}}
    update(path, mutate)
    return probe


def record_deferral(path: Path, probe: str, **fields) -> str:
    """Durably defer or resume one probe through the census writer."""
    def mutate(before):
        document = require_current_schema(before, path)
        candidate, key = set_deferral(document, probe, **fields)
        return candidate, {key: {"deferral"}}
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
def _acceptable_failures_argument(text: str) -> int:
    """`--set-acceptable-failures`'s argument, as a stored X.

    There is deliberately no `none` here any more. #1428 staged a
    nullable X while the policy was still being chosen; #1430 chose it,
    and every probe in the census now has one — "must pass every run" is
    spelled `0`, not "unset".
    """
    if text == "none":
        raise CensusError(
            f"--set-acceptable-failures takes an integer from "
            f"{MIN_ACCEPTABLE_FAILURES} through {MAX_ACCEPTABLE_FAILURES}; "
            f"there is no `none` X, because every probe in the census has a "
            f"policy — `--set-acceptable-failures "
            f"{DEFAULT_ACCEPTABLE_FAILURES}` is how \"must pass every run\" "
            f"is stated")
    try:
        value = int(text)
    except ValueError:
        raise CensusError(
            f"--set-acceptable-failures takes an integer from "
            f"{MIN_ACCEPTABLE_FAILURES} through {MAX_ACCEPTABLE_FAILURES}, "
            f"got {text!r}") from None
    return require_acceptable_failures(value, "--set-acceptable-failures")


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
    deferral = args.defer or args.resume
    # `is not None`, not truthiness: `--probe ""` was still supplied.
    if args.probe is not None and not policy and not deferral and not args.summary:
        # The second clause only when it is the mode actually selected:
        # a `--print --probe X` should not be told about a mode it did
        # not ask for.
        why = ("; --promotion-candidates reports the whole registry, "
               "because which probes qualify is the question it answers"
               if args.promotion_candidates else "")
        raise CensusError(
            "--probe is only used by --summary, --defer, --resume, "
            "--set-acceptable-failures and --set-estimate" + why)
    # The evaluation time, the horizon and the machine-readable form
    # belong to the two READING modes, and to both of them equally:
    # `--promotion-candidates` classifies a cohort as fresh or stale by
    # exactly the inputs `--summary` reports age against, so pinning
    # them is the same operation in either.
    reading = args.summary or args.promotion_candidates
    for flag, supplied in (("--as-of", args.as_of is not None),
                           ("--stale-after-days",
                            args.stale_after_days is not None),
                           ("--json", args.json_output)):
        if supplied and not reading:
            raise CensusError(
                f"{flag} is only valid with --summary or "
                f"--promotion-candidates")
    if args.justification is not None and not setting_x:
        raise CensusError(
            "--justification is only valid with --set-acceptable-failures")
    if args.clear_justification and not setting_x:
        raise CensusError(
            "--clear-justification is only valid with "
            "--set-acceptable-failures")
    if args.reason is not None and not args.defer:
        raise CensusError("--reason is only valid with --defer")
    if args.resume_when is not None and not args.defer:
        raise CensusError("--resume-when is only valid with --defer")
    # Requirements 2 and 3 prescribe contradictory writes together, so
    # this pair is refused rather than silently resolved either way.
    if args.justification is not None and args.clear_justification:
        raise CensusError(
            "--justification and --clear-justification write the same field; "
            "use one per invocation")
    if not policy:
        return None
    if setting_x and setting_estimate:
        raise CensusError(
            "--set-acceptable-failures and --set-estimate update different "
            "policy fields; use one per invocation")
    if not args.probe:
        raise CensusError("--probe KEY is required for a policy update")
    if setting_x:
        acceptable = _acceptable_failures_argument(
            args.set_acceptable_failures)
        # The three cases are decided by which FLAG was supplied, never
        # by what its text says: an in-band magic value would make some
        # legitimate justification (`none`, `keep`) unstorable, which is
        # the defect #1479 closes. `--justification` therefore stores
        # its argument verbatim, whatever it spells.
        if args.clear_justification:
            # An X above the default must say why it is there, so
            # clearing its reason in the same breath would install a
            # tolerance nobody can account for. Refused at the argument
            # layer, where it costs no census read.
            if acceptable != MIN_ACCEPTABLE_FAILURES:
                raise CensusError(
                    f"--clear-justification is valid only while setting X to "
                    f"{MIN_ACCEPTABLE_FAILURES}: an X of {acceptable} needs a "
                    f"stated reason, so clearing it would leave a tolerance "
                    f"with none")
            justification = None
        elif args.justification is None:
            justification = KEEP
        else:
            justification = args.justification
        return {
            "acceptable_failures": acceptable,
            "justification": justification,
        }
    return {"estimate": _optional_number(args.set_estimate, "--set-estimate")}


def _deferral_arguments(args) -> dict | None:
    """The `set_deferral` keywords selected by the CLI, or None."""
    if not args.defer and not args.resume:
        return None
    if not args.probe:
        raise CensusError("--probe KEY is required for a deferral update")
    if args.resume:
        return {"resume": True}
    if args.reason is None or args.resume_when is None:
        raise CensusError(
            "--defer requires both --reason and --resume-when")
    return {
        "reason": require_deferral_text(args.reason, "--reason"),
        "resume_when": require_deferral_text(
            args.resume_when, "--resume-when"),
    }


def _summary_arguments(args) -> dict:
    """The evaluation time and horizon a READING mode runs under.

    Both are INPUTS, defaulted here and nowhere deeper: the library
    reads no clock of its own, so a caller — a test, or a selection
    pass replaying a decision — always states the moment it is asking
    about.
    """
    if args.as_of is None:
        now = datetime.datetime.now(datetime.timezone.utc)
    else:
        now = parse_timestamp(args.as_of, "--as-of")
    if args.stale_after_days is None:
        horizon = DEFAULT_STALE_AFTER_SECONDS
    else:
        days = _optional_number(args.stale_after_days, "--stale-after-days")
        if days is None:
            raise CensusError(
                "--stale-after-days takes a number of days; there is no "
                "`none` horizon, because every cohort would then be fresh")
        horizon = require_horizon(days) * SECONDS_PER_DAY
    return {"now": now, "stale_after_seconds": horizon}


def _rate_text(summary: dict) -> str:
    """A cohort's combined failure rate, or why there is no number."""
    if not summary["measured"]:
        return "-"
    if summary["failure_rate"] is None:
        return "n/a"
    return f"{summary['failure_rate'] * 100:.1f}%"


def render_summary(summaries: list[dict]) -> str:
    """The human table. `--json` is the machine-readable form.

    The commit is printed IN FULL and sits last, where the widest
    column costs the fixed ones no alignment: a selection-facing row
    reports the exact hash the statistic was measured on, and an
    abbreviation is not that hash.
    """
    header = (f"{'probe':<34}{'measured (UTC)':<22}"
              f"{'age':>9}{'runs':>7}{'fail':>6}{'X':>4}{'rate':>8}"
              f"  {'tolerance':<16}{'state':<18}commit")
    lines = [header, "-" * len(header)]
    for summary in summaries:
        policy = summary["acceptable_failures"]
        acceptable = "-" if policy is None else str(policy)
        if summary["measured"]:
            commit = summary["commit_sha"]
            measured_at = summary["measured_at"]
            age = f"{summary['age_seconds'] / SECONDS_PER_DAY:.1f}d"
            runs = str(summary["requested_runs"])
            fails = str(summary["failure_count"])
            state = "stale" if summary["stale"] else "fresh"
            if summary["cohort"] == COHORT_HISTORY:
                state += " (archived)"
        else:
            commit = measured_at = age = runs = fails = "-"
            state = "unmeasured"
        if summary["deferred"] is not None:
            state = "deferred"
        lines.append(f"{summary['key']:<34}{measured_at:<22}"
                     f"{age:>9}{runs:>7}{fails:>6}{acceptable:>4}"
                     f"{_rate_text(summary):>8}"
                     f"  {summary['tolerance']:<16}{state:<18}{commit}")
    return "\n".join(lines) + "\n"


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
    group.add_argument("--summary", action="store_true",
                       help="report each probe's current statistic, the "
                            "commit it was measured on, its age and whether "
                            "it is stale")
    group.add_argument("--promotion-candidates", action="store_true",
                       help="report the manual-only probes whose measured "
                            "reliability qualifies them for CI promotion, "
                            "separated from the clean ones a non-measurable "
                            "reason still blocks; edits nothing")
    group.add_argument("--set-acceptable-failures", metavar="N",
                       help=f"store X for --probe: the failures a complete "
                            f"{POLICY_RUN_COUNT}-run measurement may show, "
                            f"as an integer from {MIN_ACCEPTABLE_FAILURES} "
                            f"through {MAX_ACCEPTABLE_FAILURES}. Above "
                            f"{MIN_ACCEPTABLE_FAILURES} it needs a stored or "
                            f"supplied --justification")
    group.add_argument("--set-estimate", metavar="SECONDS",
                       help="store the estimated worst-case duration for "
                            "--probe (a number of seconds, or `none`)")
    group.add_argument("--defer", action="store_true",
                       help="exclude --probe from de-flake selection while "
                            "preserving its measurements and classification")
    group.add_argument("--resume", action="store_true",
                       help="clear --probe's deferral so the selector may "
                            "consider it again")
    ap.add_argument("--probe",
                    help="the probe key --summary reports on, or a policy "
                         "or deferral update acts on")
    # argparse `%`-interpolates a help string, so the strftime spelling
    # of TIMESTAMP_FORMAT cannot appear in one: `%Y` raises on 3.14 and
    # would raise at `--help` time on older interpreters. The literal
    # shape is what a user types anyway.
    ap.add_argument("--as-of", metavar="TIMESTAMP",
                    help="the evaluation time --summary measures age "
                         "against, as `YYYY-MM-DDTHH:MM:SSZ` "
                         "(default: now, in UTC)")
    ap.add_argument("--stale-after-days", metavar="DAYS",
                    help=f"the age horizon at or past which --summary calls "
                         f"a cohort stale (default: "
                         f"{DEFAULT_STALE_AFTER_DAYS})")
    ap.add_argument("--json", dest="json_output", action="store_true",
                    help="print --summary as JSON instead of a table")
    ap.add_argument("--justification", default=None,
                    help="the justification stored beside X, verbatim; omit "
                         "to leave the stored one exactly as it was")
    ap.add_argument("--clear-justification", action="store_true",
                    help=f"clear the stored justification; the only way to, "
                         f"never implied by omitting --justification, and "
                         f"valid only while setting X to "
                         f"{MIN_ACCEPTABLE_FAILURES}")
    ap.add_argument("--reason",
                    help="non-blank explanation stored by --defer")
    ap.add_argument("--resume-when", dest="resume_when",
                    help="non-blank condition that makes --probe ready to "
                         "resume")
    args = ap.parse_args(argv)

    # Argument validation runs FIRST, for every operation. `--print`
    # returns without touching the filesystem, but it must not be a hole
    # through which a misused companion flag passes unreported.
    try:
        fields = _companion_arguments(args)
        deferral_fields = _deferral_arguments(args)
        summary_arguments = (_summary_arguments(args)
                             if args.summary or args.promotion_candidates
                             else {})
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
        if deferral_fields is not None:
            record_deferral(path, args.probe, **deferral_fields)
            action = "resumed" if args.resume else "deferred"
            print(f"{action} {args.probe} in {path}")
            return 0
        if fields is not None:
            record_policy(path, args.probe, **fields)
            print(f"updated the census record for {args.probe} in {path}")
            return 0
        if args.summary:
            document = load(path)
            validate_census(document, f"census {path}")
            summaries = census_summary(document, probe=args.probe,
                                       **summary_arguments)
            if args.json_output:
                print(json.dumps(summaries, indent=2, sort_keys=True))
            else:
                sys.stdout.write(render_summary(summaries))
            return 0
        if args.promotion_candidates:
            # Imported HERE, not at module scope, because the promotion
            # module imports this one: it reads fifteen storage-core
            # symbols, and this is the only place the core needs it
            # back. A module-scope import would close that into a cycle.
            # Same convention as `import jsonschema` above.
            import probe_census_promotion  # noqa: PLC0415 - deliberately imported at use
            document = load(path)
            validate_census(document, f"census {path}")
            report = probe_census_promotion.promotion_report(
                document, **summary_arguments)
            if args.json_output:
                print(json.dumps(report, indent=2, sort_keys=True))
            else:
                sys.stdout.write(
                    probe_census_promotion.render_promotion_report(report))
            return 0
        document = load(path)
        # Shape first, then inventory: a document that is not a census
        # at all should say so, rather than being reported as ninety
        # missing probes.
        validate_census(document, f"census {path}")
        # Inventory drift and the acceptable-failure policy (#1430) are
        # both reported here, in one pass, rather than raised: a person
        # fixing a census wants every row that needs attention, not the
        # first one.
        problems = validate_manifest(document) + policy_invariants(document)
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
    print(f"{path}: {len(probe_runner_registry.PROBES)} probes, inventory agrees with "
          f"probe_runner_registry.PROBES and tools/ci_probes.py")
    return 0


if __name__ == "__main__":
    sys.exit(main())
