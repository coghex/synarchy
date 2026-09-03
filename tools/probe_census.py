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

Composition (#2034, #2131)
--------------------------
This file is the COMMAND and the compatibility facade: argument
validation, dispatch, presentation selection and exception-to-exit
mapping, over owners that hold every implementation body. The five
commands, their arguments, their output, their diagnostics and their
exit codes are unchanged by that split, and so is the Python surface
every sibling tool imports from `probe_census` -- the names below are
re-exported here, from one implementation each.

  `probe_census_contract`   what a document IS: the schema identifiers
                            and shared constants, `CensusError`, the
                            declared JSON Schema (#1492), the non-finite
                            rejection, the cross-field invariants
                            (#1493), the scalar requirements, and one
                            cohort's combined statistic;
  `probe_census_records`    every PURE transformation of a document:
                            the empty record and the manifest, #1430's
                            policy as data, migration and
                            reconciliation, target lookup, and the
                            ingestion, policy and deferral mutations;
  `probe_census_summary`    the READER: which cohort is authoritative,
                            how old it is against a supplied horizon,
                            and the per-entry and whole-census
                            summaries;
  `probe_census_storage`    the only impure owner: the docs worktree,
                            the cross-process lock, path-substitution
                            refusal, the preservation and append-only
                            guards, the atomic replacement, and the ONE
                            `update()` transaction every stored mutation
                            passes through;
  `probe_census_promotion`  #2034's read-only CI-promotion report, which
                            consumes the three pure owners and never
                            reaches storage or the CLI.

The dependency runs one way and has no cycle: contract, then records,
then summary; contract and records under storage; the three pure owners
under promotion; and every owner under this facade, which no owner
imports. `--promotion-candidates` still imports the promotion owner at
its point of use, following this file's own `import jsonschema  #
noqa: PLC0415` convention.

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
import datetime
import json
import math
import os
import sys
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_runner_registry  # noqa: E402

# The compatibility surface, re-exported from ONE implementation each.
# Every name below was a top-level name of this module before #2131 and
# is imported by name from `probe_census` somewhere in the tree, so the
# imports are the whole of the re-export: nothing here re-implements,
# wraps or shadows an owner's definition.
#
# The leading-underscore names are here for the same reason the public
# ones are — they are named across module boundaries today, and #2131
# preserved rather than broke that. The census-family callers that use
# them (`probe_census_page.py`, `probe_census_promotion.py`,
# `probe_claim_selftest_census.py` and the self-test package) name their
# owner directly instead; these bindings keep an out-of-tree caller
# working.
from probe_census_contract import (  # noqa: E402,F401
    ACCEPTED_STATUS, CENSUS_RULES, CENSUS_SCHEMA, CI_ELIGIBLE, CLAIM_SCHEMA,
    COHORT_CURRENT, COHORT_HISTORY, CensusError, DEFAULT_ACCEPTABLE_FAILURES,
    DEFAULT_STALE_AFTER_DAYS, DEFAULT_STALE_AFTER_SECONDS, FULL_COMMIT_LENGTH,
    INSTALL_HINT, LEGACY, MANIFEST_SCHEMA, MANUAL_ONLY,
    MAX_ACCEPTABLE_FAILURES, MIGRATABLE_SCHEMAS, MIN_ACCEPTABLE_FAILURES,
    OUTCOME_SCHEMA, PLACEHOLDER_COMMIT, POLICY_RUN_COUNT, RECORD_SCHEMA,
    RESULT_RULES, RESULT_SCHEMA, SCHEMA_DEFINITIONS, SCHEMA_PATH,
    SECONDS_PER_DAY, SEED_SCHEMA, TIMESTAMP_FORMAT, TOLERANCE_ACCEPTABLE,
    TOLERANCE_NOT_COMPARABLE, TOLERANCE_OVER, _accepted_attempts, _cohorts,
    _non_finite_paths, _reject_non_finite, _render_tally, _require_jsonschema,
    _rule_accepted_derives_from_status, _rule_attempt_leaves_a_run_uncompleted,
    _rule_attempts_reconcile_with_samples,
    _rule_check_counts_cover_the_descriptor, _rule_check_counts_tally_runs,
    _rule_cohort_holds_one_commit, _rule_deferral_is_actionable,
    _rule_pass_run_has_no_failed_check, _rule_result_leaves_a_run_uncompleted,
    _unfinished_measurement, _validator, _zero_tally, census_invariants,
    census_record_invariants, cohort_statistic, load_schema,
    observed_check_counts, parse_timestamp, require_commit_identity,
    require_count, require_measurement_semantics, result_invariants,
    validate_census, validate_document, validate_result,
)
from probe_census_records import (  # noqa: E402,F401
    KEEP, _appendable, _archive_current, _deep_copy, _is_x, _registered_keys,
    _rows, build_manifest, classification, empty_census, find_claim,
    find_entry, find_outcome, ingest_claim, ingest_outcome, ingest_result,
    migrate_document, policy_invariants, policy_record_problems, policy_sample,
    reconcile_inventory, render_manifest, require_acceptable_failures,
    require_deferral, require_deferral_text, result_target, set_deferral,
    set_policy, summarize_attempt, summarize_sample, target_row,
    tolerance_state, validate_manifest,
)
from probe_census_storage import (  # noqa: E402,F401
    APPENDING_ASPECTS, ASPECT_FIELDS, ASPECT_LABEL, CLAIM_FIELDS,
    CREATE_DOCS_WORKTREE, CensusDurabilityUnconfirmed, DEFERRAL_FIELDS,
    DOCS_BRANCH, DocsWorktreeMissing, INVENTORY_FIELDS, LOCK_NOTE, LOCK_SUFFIX,
    MANIFEST_RELPATH, MEASUREMENT_FIELDS, OUTCOME_FIELDS, POLICY_FIELDS,
    STAGING_PREFIX, STAGING_SUFFIX, TOUCH_ANY, _append_only, _atomic_replace,
    _census_of, _check_preserved, _clear_staging, _entry_map,
    _is_initialized_x, _locked, _refuse_substituted, _sample_total,
    _worktree_records, ensure_document, load, lock_path, manifest_path,
    read_for_update, record_claim, record_deferral, record_outcome,
    record_outcome_installed, record_policy, record_result,
    record_result_installed, refuse_ci_eligible_measurement,
    require_current_schema, resolve_docs_worktree, seed, update,
)
from probe_census_summary import (  # noqa: E402,F401
    authoritative_cohort, census_summary, require_horizon, summarize_entry,
)


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
