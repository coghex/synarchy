#!/usr/bin/env python3
"""The fixtures more than one census case owner reads (#2129).

`tools/probe_census_selftest_support.py` (#2034) is still the ONE source
of the synthetic world every owner drives -- the registries and the
fixture that installs them, the scratch tree and the scratch repository,
the in-process CLI driver, the realistic result document, the fixed
evaluation moment and `expect_refusal`. That module is shared with
`tools/test_probe_census_promotion.py`, which lives OUTSIDE this
package, so it stays where it is; this one re-exports it so a case owner
needs one import rather than two, and adds the narrower half of #2129
requirement 7: the stored documents and record builders that more than
one family in this package reads.

Each is listed with the families that read it, because that -- not
tidiness -- is what puts it here instead of with an owner:

  `v1_document`         storage, policy, validation;
  `sample_record`       storage, policy (through that family's own
                        `_legacy_policy_census`);
  `attempt_record`      storage, policy (the same route), outcomes;
  `stored_v3_document`  validation, outcomes;
  `stored_v4_document`  validation, outcomes;
  `rich_census`         validation, outcomes;
  `_alpha`              validation (through its stored-case mutators),
                        outcomes;
  `measurement`         cohort, policy;
  `summary_of`          cohort, policy.

A fixture only ONE family reads stays with that family and is
deliberately not here: `storage` keeps `expect_refusal_kind`,
`staging_residue` and `CONTENDER`, `policy` keeps
`_legacy_policy_census`, `validation` keeps the adversarial sweep's
values, the stored-case and result-case mutators and both schema
harnesses, `cohort` keeps `COMMIT_C`, and `outcomes` keeps
`outcome_record`.

This module imports no case owner, so the dependency direction through
the package runs one way -- support, then the five owners, then the
facade -- and nothing here runs a case.
"""

from __future__ import annotations

import copy
import json
import sys
from pathlib import Path

#: `tools/` -- this package's parent, and where `probe_census`,
#: `selftestlib` and `probe_census_selftest_support` live. A module
#: INSIDE the package does not inherit the entry `python3
#: tools/<name>.py` installs, so resolving it once here is what lets a
#: focused `--family storage` run import the production code in a fresh
#: interpreter.
TOOLS_DIR = Path(__file__).resolve().parent.parent

if str(TOOLS_DIR) not in sys.path:
    sys.path.insert(0, str(TOOLS_DIR))

# Both need TOOLS_DIR on sys.path, which is why they follow it.
import probe_census  # type: ignore  # noqa: E402
from probe_census_selftest_support import (  # noqa: E402
    COMMIT_A, COMMIT_B, DAY, NOW, SYNTHETIC, at, cli, cli_repo, expect,
    expect_refusal, registry, result_document, scratch, seeded, unchanged,
)

__all__ = [
    "COMMIT_A", "COMMIT_B", "DAY", "NOW", "SYNTHETIC", "TOOLS_DIR", "_alpha",
    "at", "attempt_record", "cli", "cli_repo", "expect", "expect_refusal",
    "measurement", "probe_census", "registry", "result_document",
    "rich_census", "sample_record", "scratch", "seeded", "stored_v3_document",
    "stored_v4_document", "summary_of", "unchanged", "v1_document",
]


def v1_document() -> dict:
    """A `probe-census/v1` seed exactly as #1425 writes one."""
    return {
        "schema": "probe-census/v1",
        "probes": [
            {"key": "alpha", "script": "alpha_probe.py",
             "classification": "manual-only", "protocol": "legacy"},
            {"key": "beta", "script": "beta_probe.py",
             "classification": "ci-eligible", "protocol": "probe-result/v1"},
        ],
    }


def sample_record(mark: str, commit: str = COMMIT_A) -> dict:
    """A schema-valid durable sample, tagged by its retained artifact.

    Fixtures used to stand a sample in as `{"tag": ...}`. Since #1492
    declared the record's shape, stored state has to BE a sample — so
    these come from the real summarizer and carry one distinguishing
    value each.
    """
    record = probe_census.summarize_sample(result_document(commit=commit))
    record["retained_artifacts"] = [f"/tmp/artifacts/{mark}"]
    return record


def attempt_record(mark: str, commit: str = COMMIT_A) -> dict:
    """A schema-valid durable attempt, tagged by its error text."""
    record = probe_census.summarize_attempt(
        result_document(commit=commit), True)
    record["error"] = mark
    return record


def stored_v3_document() -> dict:
    """A `probe-census/v3` census exactly as #1434 left one.

    Seven-field records, `claims` but no `outcomes`. Spelled out here
    for the same reason `stored_v2_document` is: it describes migration
    INPUT, and deriving it from the current `empty_census()` would
    silently start testing the current shape the moment the record grows
    another field.
    """
    return {
        "schema": "probe-census/v3",
        "probes": [{
            "key": "alpha", "script": "alpha_probe.py",
            "classification": "manual-only", "protocol": "legacy",
            "census": {
                "acceptable_failures": 2,
                "acceptable_failures_justification": "two known races",
                "estimated_worst_case_seconds": 480,
                "current": None,
                "history": [],
                "attempts": [],
                "claims": [],
            },
        }],
    }


def stored_v4_document() -> dict:
    """A `probe-census/v4` census exactly as #1439 left one.

    Eight-field records, through `outcomes` but no `deferred`. Like the
    older fixtures, this is migration INPUT and must not derive from the
    current record shape.
    """
    document = stored_v3_document()
    document["schema"] = probe_census.OUTCOME_SCHEMA
    document["probes"][0]["census"]["outcomes"] = []
    return document


def rich_census() -> dict:
    """A current-schema census with real accumulated data on its first row.

    Consistent under #1493's cross-field invariants, which is what keeps
    every case built on it honest: a base document that were already
    inconsistent would be refused for its OWN defect, and each case
    would then pass without ever exercising the mutation it applies.
    So the two retained samples are matched by two accepted attempts —
    one per commit, in the order they were ingested — and the archived
    cohort's sample really is from that cohort's commit.
    """
    sample = probe_census.summarize_sample(result_document())
    attempt = probe_census.summarize_attempt(result_document(), True)
    archived = probe_census.summarize_sample(result_document(commit=COMMIT_B))
    archived_attempt = probe_census.summarize_attempt(
        result_document(commit=COMMIT_B), True)

    def row(key, census):
        return {"key": key, "script": f"{key}_probe.py",
                "classification": "manual-only", "protocol": "legacy",
                "census": census}

    return {
        "schema": probe_census.CENSUS_SCHEMA,
        "probes": [
            row("alpha", {"acceptable_failures": 2,
                          "acceptable_failures_justification": "two races",
                          "estimated_worst_case_seconds": 480,
                          "current": {"commit_sha": COMMIT_A,
                                      "samples": [copy.deepcopy(sample)]},
                          "history": [{"commit_sha": COMMIT_B,
                                       "samples": [copy.deepcopy(archived)]}],
                          "attempts": [copy.deepcopy(archived_attempt),
                                       copy.deepcopy(attempt)],
                          "claims": [],
                          "outcomes": [],
                          "deferred": None}),
            row("beta", probe_census.empty_census()),
            row("gamma", probe_census.empty_census()),
        ],
    }


def _alpha(document: dict) -> dict:
    """The census record of `rich_census()`'s measured row."""
    return document["probes"][0]["census"]


def measurement(commit=COMMIT_A, *, runs=2, failures=1, age_days=0.0,
                probe="alpha", **overrides):
    """One accepted result: a batch of `runs` with `failures` of them bad."""
    return result_document(
        probe=probe, commit=commit, requested_runs=runs, completed_runs=runs,
        failure_count=failures,
        failure_rate=None if runs == 0 else failures / runs,
        timestamp_utc=at(age_days), **overrides)


def summary_of(path: Path, probe="alpha", *, now=NOW,
               stale_after_seconds=14 * DAY) -> dict:
    document = json.loads(path.read_text(encoding="utf-8"))
    return probe_census.census_summary(
        document, now=now, stale_after_seconds=stale_after_seconds,
        probe=probe)[0]
