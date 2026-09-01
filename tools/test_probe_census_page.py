#!/usr/bin/env python3
"""Self-test for the manual-only probe census page and its audit (#1431).

Deterministic, engine-free, GPU-free and offline. Every case runs
against synthetic censuses and a synthetic probe registry in a
throwaway temporary tree: nothing boots an engine, runs a registered
probe, reads the developer's real `docs-wip` worktree, or reads a
clock — the as-of time is injected, so an age case is the same age case
on every machine and at every hour.

The real `tools/probe_census_page.py` is imported and driven, with
`probe_runner_registry.PROBES`, `ci_probes.CI_ELIGIBLE` and
`probe_flake.PROTOCOL_PROBES` pointed at a synthetic registry, so this
exercises the shipped code paths rather than a copy.

What it covers, following #1431's gate:

* the whole-registry manifest filtered down to the manual-only page,
  and the CI-eligible rows that must NOT appear;
* legacy entries, and the three distinct measurement states;
* several reason categories on one probe, in DECLARED order;
* promotion: the row leaves the page while its manifest entry, policy
  and archived history stay untouched, and `--record` then refuses a
  further measurement for it;
* missing, duplicate and extra page rows, each diagnosed on its own;
* a tampered value in every audited column;
* the source-manifest gate in front of both operations;
* byte-stable rendering, and the CLI's exit codes against a real
  two-worktree scratch repository.

Usage:
  python3 tools/test_probe_census_page.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import datetime
import io
import json
import shutil
import subprocess
import sys
import tempfile
from contextlib import contextmanager
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import ci_probes  # type: ignore  # noqa: E402
import probe_census  # type: ignore  # noqa: E402
import probe_census_page as page  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
import probe_engine  # type: ignore  # noqa: E402
import probe_runner_registry  # type: ignore  # noqa: E402

import selftest  # noqa: E402
from selftest import FAILURES, expect  # noqa: E402


COMMIT_A = "a" * 40
COMMIT_B = "b" * 40
NOW = datetime.datetime(2026, 8, 21, 12, 0, 0, tzinfo=datetime.timezone.utc)
DAY = probe_census.SECONDS_PER_DAY


def expect_refusal(call, msg: str, *fragments: str) -> None:
    """`call` refuses with a controlled `CensusError` naming each fragment."""
    try:
        call()
    except probe_census.CensusError as error:
        missing = [f for f in fragments if f not in str(error)]
        expect(not missing,
               msg if not missing else
               f"{msg} (message {str(error)!r} is missing {missing})")
        return
    except Exception as error:  # noqa: BLE001 - an uncontrolled failure IS the bug
        expect(False, f"{msg} (raised {type(error).__name__}: {error})")
        return
    expect(False, f"{msg} (nothing was raised)")


# ==========================================================================
# Fixtures
# ==========================================================================
# Four probes: one CI-eligible, and three manual-only covering the three
# measurement states. `zeta` sorts last on purpose, so "stable key order"
# is not satisfied by insertion order.
SYNTHETIC = [
    ("alpha", "alpha_probe.py", "a migrated, measured manual-only probe"),
    ("beta", "beta_probe.py", "the CI-eligible one"),
    ("gamma", "gamma_probe.py", "a legacy manual-only probe"),
    ("zeta", "zeta_probe.py", "a migrated but unmeasured manual-only probe"),
]
PROTOCOL = {"alpha": "probe-result/v1", "beta": "probe-result/v1",
            "zeta": "probe-result/v1"}
REASONS = {
    # Deliberately NOT alphabetical, and not in key order: #1440's
    # declared order is what the page must render.
    "alpha": (ci_probes.Reason(ci_probes.SLOW_WORLDGEN, "a real world"),
              ci_probes.Reason(ci_probes.FLAKY, "AI timing")),
    "gamma": (ci_probes.Reason(ci_probes.NEEDS_GPU, "no GPU on the runner"),),
    "zeta": (ci_probes.Reason(ci_probes.SCENARIO_HEAVY, "walks its legs"),),
}


@contextmanager
def registry(probes=None, ci_eligible=("beta",), protocol=None, reasons=None):
    """The four live registries, pointed at the synthetic set for one case.

    `MANUAL_ONLY_REASONS` moves with the others because the page reads
    it as a source: leaving the shipped mapping in place would make the
    CLI cases look for a reason record for `alpha`.
    """
    saved = (probe_runner_registry.PROBES, ci_probes.CI_ELIGIBLE,
             probe_flake.PROTOCOL_PROBES, ci_probes.MANUAL_ONLY_REASONS)
    probe_runner_registry.PROBES = list(SYNTHETIC if probes is None else probes)
    ci_probes.CI_ELIGIBLE = set(ci_eligible)
    probe_flake.PROTOCOL_PROBES = dict(PROTOCOL if protocol is None
                                       else protocol)
    ci_probes.MANUAL_ONLY_REASONS = dict(REASONS if reasons is None
                                         else reasons)
    try:
        yield
    finally:
        (probe_runner_registry.PROBES, ci_probes.CI_ELIGIBLE,
         probe_flake.PROTOCOL_PROBES,
         ci_probes.MANUAL_ONLY_REASONS) = saved


@contextmanager
def scratch(prefix="probe-census-page-test-"):
    root = Path(tempfile.mkdtemp(prefix=prefix))
    try:
        yield root
    finally:
        shutil.rmtree(root, ignore_errors=True)


def measurement(commit: str, *, runs: int, failures: int, age_days: float,
                probe: str = "alpha") -> dict:
    """A complete, invariant-satisfying `probe-flake-result/v1` document.

    Built rather than hand-written so every fixture below is state a
    real harness could have produced: the per-run outcomes, the check
    tally and the aggregate counts all agree, which is what #1493's
    cross-field invariants require of anything the census stores.
    """
    when = NOW - datetime.timedelta(days=age_days)
    outcomes = ["FAIL"] * failures + ["PASS"] * (runs - failures)
    return {
        "schema": probe_census.RESULT_SCHEMA,
        "probe": probe,
        "status": "ok",
        "error": None,
        "requested_runs": runs,
        "completed_runs": runs,
        "runs": [{"index": index + 1, "port": 9100 + index,
                  "outcome": outcome, "elapsed_seconds": 1.0,
                  "checks": {"only": outcome}, "artifact_dir": None}
                 for index, outcome in enumerate(outcomes)],
        "error_run": None,
        "checks": [{"id": "only", "label": "the only check"}],
        "check_counts": {"only": {"PASS": runs - failures, "FAIL": failures,
                                  "MISSING": 0}},
        "failure_count": failures,
        "failure_rate": None if runs == 0 else failures / runs,
        "timeout_count": 0,
        "worst_elapsed_seconds": 1.0,
        "total_elapsed_seconds": float(runs),
        "timestamp_utc": when.strftime(probe_census.TIMESTAMP_FORMAT),
        "commit_sha": commit,
        "rts_capabilities": 4,
        "peak_concurrency": 1,
        "artifact_root": "/tmp/artifacts",
        "invocation_dir": "/repo",
        "retained_artifacts": [],
    }


def harness_error(commit: str, *, probe: str = "alpha",
                  age_days: float = 2.0) -> dict:
    """A well-formed harness error: it contributes no sample."""
    document = measurement(commit, runs=2, failures=0, age_days=age_days,
                           probe=probe)
    document.update({
        "status": "harness-error",
        "error": "run 2 emitted a duplicate event",
        "completed_runs": 1,
        "runs": document["runs"][:1],
        "check_counts": {"only": {"PASS": 1, "FAIL": 0, "MISSING": 0}},
        "failure_rate": None,
        "total_elapsed_seconds": 1.0,
        "error_run": {"index": 2, "port": 9101, "outcome": "HARNESS_ERROR",
                      "elapsed_seconds": 0.5, "checks": {},
                      "artifact_dir": None},
    })
    return document


def census(**overrides) -> dict:
    record = probe_census.empty_census()
    record.update(overrides)
    return record


def measured(*results, **overrides) -> dict:
    """A census record whose current cohort holds exactly `results`.

    The accepted attempt log is derived from the same documents, so the
    fixture satisfies #1493's "every accepted attempt is one retained
    sample" invariant by construction rather than by hand.
    """
    record = census(**overrides)
    record["current"] = {
        "commit_sha": results[0]["commit_sha"],
        "samples": [probe_census.summarize_sample(r) for r in results],
    }
    record["attempts"] = [probe_census.summarize_attempt(r, True)
                          for r in results]
    return record


def document(*, alpha=None, gamma=None, zeta=None) -> dict:
    """A v2 census over the synthetic registry, one row per probe."""
    def row(key, script, classification, protocol, record):
        return {"key": key, "script": script,
                "classification": classification, "protocol": protocol,
                "census": record if record is not None else census()}
    return {
        "schema": probe_census.CENSUS_SCHEMA,
        "probes": [
            row("alpha", "alpha_probe.py", "manual-only", "probe-result/v1",
                alpha if alpha is not None else measured(
                    measurement(COMMIT_A, runs=10, failures=3, age_days=2.0),
                    measurement(COMMIT_A, runs=4, failures=0, age_days=1.5),
                    estimated_worst_case_seconds=480)),
            row("beta", "beta_probe.py", "ci-eligible", "probe-result/v1",
                census()),
            row("gamma", "gamma_probe.py", "manual-only", "legacy", gamma),
            row("zeta", "zeta_probe.py", "manual-only", "probe-result/v1",
                zeta),
        ],
    }


def rendered(doc=None, *, as_of=NOW) -> str:
    return page.render_page(doc if doc is not None else document(),
                            as_of=as_of, reasons=REASONS)


def rows_of(text: str) -> dict:
    _fields, rows = page.parse_page(text)
    return {row["probe"]: row for row in rows}


def audit(text, doc=None) -> list[str]:
    return page.audit_page(text, doc if doc is not None else document(),
                           reasons=REASONS)


def retable(text: str, mutate) -> str:
    """`text` with its data rows replaced by `mutate(rows)`.

    Rebuilt through the real renderer's own table helper, so a tampered
    page stays structurally a page and the audit is forced to find the
    tampering in a CELL rather than in the layout.
    """
    head, rows = page.parse_page(text)
    body = page._table(mutate([dict(row) for row in rows]))
    keep = [line for line in text.splitlines() if not line.startswith("|")]
    del head
    return "\n".join(keep + body) + "\n"


# ==========================================================================
def test_filtering() -> None:
    print("\n-- the page is the manual-only subset of the whole manifest --")
    with registry():
        expect(page.manual_only_keys() == ["alpha", "gamma", "zeta"],
               "the row set is ALL_KEYS - CI_ELIGIBLE, in stable key order")
        text = rendered()
        rows = rows_of(text)
        expect(list(rows) == ["alpha", "gamma", "zeta"],
               "the rendered page holds exactly those rows, in that order")
        expect("beta" not in rows,
               "the CI-eligible probe takes no row, though the manifest "
               "keeps it")
        expect(len(document()["probes"]) == 4,
               "and the source manifest still carries all four probes")
        fields, _ = page.parse_page(text)
        expect(fields[page.FIELD_REGISTERED] == "4"
               and fields[page.FIELD_CI_ELIGIBLE] == "1"
               and fields[page.FIELD_MANUAL_ONLY] == "3",
               "the header counts the whole registry, not just the page")
        expect(fields[page.FIELD_AS_OF] == "2026-08-21T12:00:00Z",
               "and records the UTC as-of time every age is measured from")
        expect(audit(text) == [], "a freshly generated page audits clean")
        expect(rendered() == text,
               "rendering is byte-stable across runs on unchanged inputs")


# ==========================================================================
def test_states_and_cells() -> None:
    print("\n-- the three measurement states, and every cell beside them --")
    with registry():
        rows = rows_of(rendered())
        alpha, gamma, zeta = rows["alpha"], rows["gamma"], rows["zeta"]

        expect(alpha["state"] == page.STATE_MEASURED,
               "a migrated probe with a current cohort is `measured`")
        # 3 failures over 14 requested runs pooled: an unweighted mean of
        # the two samples' own rates would report 15.0%.
        expect(alpha["rate"] == "21.4% (3/14)",
               "its rate pools the cohort's counts rather than averaging "
               "the samples' stored rates")
        expect(alpha["commit"] == COMMIT_A,
               "the tested commit is reported in full, never abbreviated")
        expect(alpha["age"] == "1.5 d",
               "age runs from the cohort's NEWEST sample to the as-of time")
        expect(alpha["worst case"] == "480 s" and alpha["X"] == "0",
               "the duration estimate and X come from the record")

        expect(gamma["state"] == page.STATE_NOT_MEASURABLE,
               "a legacy probe is `not yet measurable`")
        expect(gamma["protocol"] == "legacy",
               "and its protocol status says legacy on the row")
        expect(zeta["state"] == page.STATE_NOT_MEASURED,
               "a migrated probe with no cohort is `not yet measured`")
        expect(all(row[column] == page.ABSENT
                   for row in (gamma, zeta)
                   for column in ("rate", "commit", "age")),
               "neither invents a rate, a commit or an age")
        expect(gamma["worst case"] == page.DURATION_UNKNOWN,
               "an absent duration estimate renders as `unknown`, not 0")

        # X is nullable in a census written before #1430 chose the policy.
        unset = document(zeta=census(acceptable_failures=None))
        expect(rows_of(rendered(unset))["zeta"]["X"] == page.X_UNSET,
               "an unset X renders as `unset`, never as a tolerance of 0")

        # A cohort whose samples requested no runs has no denominator,
        # which is not the same observation as a rate of zero.
        empty = document(zeta=measured(
            measurement(COMMIT_B, runs=0, failures=0, age_days=3.0)))
        expect(rows_of(rendered(empty))["zeta"]["rate"] == "n/a (0/0)",
               "a cohort with no denominator reports `n/a`, not 0.0%")

        # A future-anchored cohort is the freshest thing there is.
        future = document(zeta=measured(
            measurement(COMMIT_B, runs=2, failures=1, age_days=-5.0)))
        expect(rows_of(rendered(future))["zeta"]["age"] == "0.0 d",
               "age is clamped at zero rather than going negative")

        # Cohort-first: a legacy row carrying a stored cohort reports it.
        # `probe_flake` cannot produce one, but hiding a real measurement
        # behind "not yet measurable" would be a lie.
        measured_legacy = document(gamma=measured(
            measurement(COMMIT_B, runs=5, failures=1, age_days=4.0)))
        row = rows_of(rendered(measured_legacy))["gamma"]
        expect(row["state"] == page.STATE_MEASURED and row["rate"] == "20.0% (1/5)"
               and row["protocol"] == "legacy",
               "a legacy row that does hold a cohort reports the cohort, "
               "while still declaring its legacy protocol")


# ==========================================================================
def test_reason_categories() -> None:
    print("\n-- every reason category, in the order #1440 declares --")
    with registry():
        rows = rows_of(rendered())
        expect(rows["alpha"]["reasons"] == "slow/worldgen-heavy, flaky",
               "a probe excluded on several counts lists them all, in "
               "DECLARED order rather than sorted")
        expect(rows["gamma"]["reasons"] == "needs-gpu",
               "a single-reason probe lists exactly its one category")

        # Reordering the declaration reorders the cell, so the order is
        # really read from the registry and not incidentally alphabetical.
        flipped = dict(REASONS)
        flipped["alpha"] = tuple(reversed(REASONS["alpha"]))
        text = page.render_page(document(), as_of=NOW, reasons=flipped)
        expect(rows_of(text)["alpha"]["reasons"] == "flaky, slow/worldgen-heavy",
               "reversing the declaration reverses the rendered order")

        expect_refusal(
            lambda: page.render_page(document(), as_of=NOW,
                                     reasons={k: v for k, v in REASONS.items()
                                              if k != "gamma"}),
            "a manual-only probe with no recorded reason refuses",
            "gamma", "no manual-only reason")
        expect_refusal(
            lambda: page.render_page(document(), as_of=NOW,
                                     reasons=dict(REASONS, gamma=())),
            "so does an empty reason tuple, via the shared shape validator",
            "unusable")


# ==========================================================================
def test_promotion() -> None:
    print("\n-- promotion removes the row, never the manifest entry --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        probe_census.ensure_document(path)
        result = measurement(COMMIT_B, runs=2, failures=0, age_days=2.0,
                             probe="gamma")
        probe_census.record_result(path, result)
        stored = json.loads(path.read_text(encoding="utf-8"))
        before = [r for r in stored["probes"] if r["key"] == "gamma"][0]
        expect(before["census"]["current"]["commit_sha"] == COMMIT_B,
               "the manual-only probe accumulates a cohort while it is one")
        expect("gamma" in rows_of(page.render_page(stored, as_of=NOW,
                                                   reasons=REASONS)),
               "and takes a page row")

        with registry(ci_eligible=("beta", "gamma")):
            promoted = probe_census.ensure_document(path)
            row = [r for r in promoted["probes"] if r["key"] == "gamma"][0]
            expect(row["classification"] == "ci-eligible"
                   and row["census"]["current"] is None
                   and len(row["census"]["history"]) == 1
                   and row["census"]["history"][0]["commit_sha"] == COMMIT_B,
                   "promotion keeps the manifest entry and archives the "
                   "cohort into retained history")
            expect(len(row["census"]["attempts"]) == 1,
                   "and keeps the attempt log")
            text = page.render_page(promoted, as_of=NOW, reasons=REASONS)
            expect("gamma" not in rows_of(text),
                   "the promoted probe leaves the page")
            fields, _ = page.parse_page(text)
            expect(fields[page.FIELD_REGISTERED] == "4"
                   and fields[page.FIELD_CI_ELIGIBLE] == "2"
                   and fields[page.FIELD_MANUAL_ONLY] == "2",
                   "and the header counts move with it")
            expect(page.audit_page(text, promoted,
                                   reasons=REASONS) == [],
                   "the smaller page audits clean against the same manifest")

            # #1431's storage half: no further samples, ever.
            unchanged = path.read_bytes()
            expect_refusal(
                lambda: probe_census.record_result(path, result),
                "recording a further measurement for a CI-eligible probe "
                "refuses",
                "gamma", "CI-eligible")
            expect(path.read_bytes() == unchanged,
                   "and refuses without mutating one byte of the census")
            expect_refusal(
                lambda: probe_census.record_result(
                    path, harness_error(COMMIT_B, probe="gamma")),
                "a harness error for a CI-eligible probe is refused too",
                "gamma", "CI-eligible")
            expect(path.read_bytes() == unchanged,
                   "leaving the census byte-identical again")

        expect(probe_census.record_result(path, result) == "gamma",
               "the same result is accepted again once the probe is "
               "manual-only, so the refusal is LIVE eligibility, not a "
               "property of the document")


# ==========================================================================
def test_row_set_findings() -> None:
    print("\n-- missing, duplicate and extra rows, diagnosed separately --")
    with registry():
        text = rendered()

        missing = retable(text, lambda rows: [r for r in rows
                                              if r["probe"] != "gamma"])
        found = audit(missing)
        expect(found == ["missing row for probe 'gamma'"],
               "a dropped row is reported as missing, and as nothing else")

        duplicated = retable(text, lambda rows: rows + [dict(rows[0])])
        found = audit(duplicated)
        expect(any("duplicate rows for probe 'alpha'" in f and "1" in f
                   and "4" in f for f in found),
               "a repeated row is reported as a duplicate, naming both rows")
        expect(not any("missing row" in f or "extra row" in f
                       for f in found),
               "and is not also mis-reported as missing or extra")

        extra = retable(text, lambda rows: rows + [dict(rows[0],
                                                        probe="beta")])
        found = audit(extra)
        expect(any("extra row for probe 'beta'" in f and "CI-eligible" in f
                   for f in found),
               "a CI-eligible row is extra, and the finding says why")
        unregistered = retable(text, lambda rows: rows + [dict(rows[0],
                                                               probe="nope")])
        expect(any("extra row for probe 'nope'" in f
                   and "not registered" in f
                   for f in audit(unregistered)),
               "an unregistered row is extra for a different, named reason")

        reordered = retable(text, lambda rows: list(reversed(rows)))
        found = audit(reordered)
        expect(any("not in stable key order" in f for f in found),
               "rows out of key order are reported")
        expect(not any("missing row" in f or "extra row" in f
                       or "duplicate rows" in f for f in found),
               "and reordering alone reports nothing else")


# ==========================================================================
def test_cell_findings() -> None:
    print("\n-- every audited column catches a tampered value --")
    with registry():
        text = rendered()
        tampers = {
            "protocol": "probe-result/v1",
            "reasons": "flaky",
            "state": page.STATE_MEASURED,
            "rate": "0.0% (0/10)",
            "X": "9",
            "worst case": "1 s",
            "commit": COMMIT_B,
            "age": "99.0 d",
        }
        for column, value in tampers.items():
            def mutate(rows, column=column, value=value):
                return [dict(row, **{column: value})
                        if row["probe"] == "gamma" else row for row in rows]
            found = audit(retable(text, mutate))
            expect(any(f"probe 'gamma': page {column} " in f for f in found),
                   f"a hand-edited `{column}` cell is reported")
            expect(len(found) == 1,
                   f"and reported once, naming only `{column}`")

        # An age is only correct RELATIVE to the declared as-of, so moving
        # the as-of alone must invalidate the ages rendered under the old
        # one rather than being accepted as a cosmetic header edit.
        moved = text.replace("2026-08-21T12:00:00Z", "2026-08-25T12:00:00Z")
        expect(any("page age " in f for f in audit(moved)),
               "editing the as-of without regenerating invalidates the ages")
        expect(audit(rendered(as_of=NOW + datetime.timedelta(days=4))) == [],
               "while a page regenerated at that as-of audits clean")


# ==========================================================================
def test_source_gate() -> None:
    print("\n-- the source manifest is validated before anything else --")
    with registry():
        text = rendered()
        for name, mutate in (
            ("a missing manifest row",
             lambda d: {**d, "probes": [r for r in d["probes"]
                                        if r["key"] != "zeta"]}),
            ("a duplicated manifest row",
             lambda d: {**d, "probes": d["probes"] + [d["probes"][0]]}),
            ("an unregistered manifest row",
             lambda d: {**d, "probes": d["probes"]
                        + [{"key": "ghost", "script": "ghost_probe.py",
                            "classification": "manual-only",
                            "protocol": "legacy",
                            "census": census()}]}),
            ("a stale classification",
             lambda d: {**d, "probes": [dict(r, classification="ci-eligible")
                                        if r["key"] == "alpha" else r
                                        for r in d["probes"]]}),
            ("a stale protocol status",
             lambda d: {**d, "probes": [dict(r, protocol="legacy")
                                        if r["key"] == "alpha" else r
                                        for r in d["probes"]]}),
        ):
            found = page.audit_page(text, mutate(document()), reasons=REASONS)
            expect(found and all("source census" in f for f in found),
                   f"{name} refuses on the manifest, before any row is "
                   f"compared")

        expect(page.source_problems({"schema": "probe-census/v1",
                                     "probes": []}),
               "and a document that is not a current census is reported as "
               "shape drift, not as three missing probes")

        expect_refusal(
            lambda: page.render_page({"schema": probe_census.CENSUS_SCHEMA,
                                      "probes": [{"key": "alpha"}]},
                                     as_of=NOW, reasons=REASONS),
            "generating from a row with no census record refuses",
            "alpha")
        expect_refusal(lambda: page.render_page(document(), as_of="now",
                                                reasons=REASONS),
                       "a naive or non-datetime as-of refuses",
                       "timezone-aware")


# ==========================================================================
def test_parse_refusals() -> None:
    print("\n-- a page that is not a generated page says so, once --")
    with registry():
        text = rendered()
        cases = [
            ("no table at all", "# Manual-only probe census\n", "no table"),
            ("a foreign header",
             text.replace("| probe ", "| key   ", 1), "table header"),
            ("a short row",
             text.replace(f"| alpha ", "| alpha |\n| alpha ", 1), "cells"),
        ]
        for name, broken, fragment in cases:
            found = audit(broken)
            expect(len(found) == 1 and fragment in found[0],
                   f"{name} is one structural finding ({fragment})")
        for field in (page.FIELD_AS_OF, page.FIELD_MANUAL_ONLY):
            # First-wins would accept a page whose ages were computed
            # from one as-of while a second, contradicting one sits
            # right below it.
            repeated = text.replace(
                "\n| probe",
                f"\n- **{field}:** 1999-01-01T00:00:00Z\n\n| probe", 1)
            found = audit(repeated)
            expect(len(found) == 1 and f"`{field}` more than once" in found[0],
                   f"a page declaring `{field}` twice is refused, not "
                   f"silently read first-wins")

        headerless = "\n".join(line for line in text.splitlines()
                               if not line.startswith(f"- **{page.FIELD_AS_OF}"))
        found = audit(headerless + "\n")
        expect(len(found) == 1 and page.FIELD_AS_OF in found[0],
               "a page with no as-of is refused rather than aged against now")
        unreadable = text.replace("2026-08-21T12:00:00Z", "yesterday")
        found = audit(unreadable)
        expect(len(found) == 1 and "yesterday" in found[0],
               "an unreadable as-of is refused, naming the offending token")


# ==========================================================================
@contextmanager
def cli_repo():
    """A scratch repository with a real `docs-wip` worktree beside it."""
    with scratch("probe-census-page-cli-") as root:
        main = root / "main"
        main.mkdir()
        def git(*argv, cwd=main):
            subprocess.run(["git", *argv], cwd=cwd, check=True,
                           capture_output=True, text=True)
        git("init", "-q", "-b", "master")
        git("config", "user.email", "test@example.com")
        git("config", "user.name", "Test")
        (main / "seed.txt").write_text("seed\n", encoding="utf-8")
        git("add", "seed.txt")
        git("commit", "-qm", "seed")
        docs = root / "docs-wt"
        git("worktree", "add", "-q", "-b", probe_census.DOCS_BRANCH,
            str(docs), "master")
        saved = probe_engine.REPO_ROOT
        probe_engine.REPO_ROOT = str(main)
        try:
            yield docs
        finally:
            probe_engine.REPO_ROOT = saved


def run(*argv) -> tuple[int, str]:
    """`page.main(argv)`, with its diagnostics captured."""
    stderr = io.StringIO()
    with contextlib.redirect_stderr(stderr):
        code = page.main(list(argv))
    return code, stderr.getvalue()


def test_cli() -> None:
    print("\n-- the CLI, against a real two-worktree scratch repository --")
    with registry(), cli_repo() as docs:
        target = docs / page.PAGE_RELPATH
        manifest = docs / probe_census.MANIFEST_RELPATH

        expect(run("--audit")[0] == 2,
               "auditing before the census exists is the same actionable "
               "exit 2 probe_census gives for an unreadable manifest")
        probe_census.ensure_document(manifest)

        # The source manifest is validated before the page is even read,
        # for BOTH operations: a stale manifest beside a missing page
        # must name the manifest, which is the thing actually wrong.
        good = manifest.read_bytes()
        drifted = json.loads(good.decode("utf-8"))
        for row in drifted["probes"]:
            if row["key"] == "beta":
                row["classification"] = "manual-only"
        manifest.write_text(json.dumps(drifted, indent=2, sort_keys=True),
                            encoding="utf-8")
        code, err = run("--audit")
        expect(code == 1 and "source census" in err and "beta" in err,
               "a stale manifest beside a MISSING page reports the manifest")
        expect("unreadable" not in err,
               "and not the missing page, which is the one diagnosis that "
               "names nothing wrong")
        code, err = run("--generate")
        expect(code == 1 and "source census" in err and not target.exists(),
               "--generate refuses on the same stale manifest, writing no "
               "page at all")
        manifest.write_bytes(good)

        expect(run("--audit")[0] == 1,
               "auditing before the page exists exits 1")
        expect(not target.exists(),
               "and creates nothing while refusing")

        expect(run("--generate", "--as-of", "2026-08-21T12:00:00Z")[0] == 0,
               "--generate writes the page and exits 0")
        expect(target.is_file(), f"the page lands at {page.PAGE_RELPATH}")
        first = target.read_bytes()
        expect(run("--audit")[0] == 0,
               "and the page it wrote audits clean")
        expect(run("--generate", "--as-of", "2026-08-21T12:00:00Z")[0] == 0
               and target.read_bytes() == first,
               "regenerating at the same as-of reproduces the same bytes")
        expect(not [p for p in (docs / "docs").iterdir()
                    if p.name.startswith(page.PAGE_STAGING_PREFIX)],
               "leaving no staging residue behind")

        target.write_text(target.read_text(encoding="utf-8").replace(
            "needs-gpu", "flaky"), encoding="utf-8")
        expect(run("--audit")[0] == 1, "a hand-edited page exits 1")
        expect(run("--generate", "--as-of", "2026-08-21T12:00:00Z")[0] == 0
               and target.read_bytes() == first,
               "and --generate restores it")

        expect(run("--audit", "--as-of", "2026-08-21T12:00:00Z")[0] == 1,
               "--as-of outside --generate is reported, not ignored")
        expect(run("--generate", "--as-of", "yesterday")[0] == 1,
               "an unreadable --as-of refuses instead of falling back to now")

        # No docs worktree at all is the actionable exit 2, not a silent
        # write into the primary checkout.
        subprocess.run(["git", "worktree", "remove", "--force", str(docs)],
                       cwd=probe_engine.REPO_ROOT, check=True,
                       capture_output=True, text=True)
        expect(run("--generate")[0] == 2,
               "a missing docs worktree exits 2")


# ==========================================================================
def main() -> int:
    selftest.parse_verbose()
    for test in (test_filtering, test_states_and_cells, test_reason_categories,
                 test_promotion, test_row_set_findings, test_cell_findings,
                 test_source_gate, test_parse_refusals, test_cli):
        test()
    print()
    if FAILURES:
        print(f"{len(FAILURES)} FAILED:")
        for message in FAILURES:
            print(f"  - {message}")
        return selftest.concluded(1)
    return selftest.concluded(0, "probe_census_page self-test: all cases pass")


if __name__ == "__main__":
    sys.exit(main())
