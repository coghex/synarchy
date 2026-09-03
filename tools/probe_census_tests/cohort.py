#!/usr/bin/env python3
"""Cohort semantics, staleness, and the summary reader (#2129).

Nine groups, in the aggregate's order: #1429's current statistic and its
age. Same-commit batches accumulate into one cohort rather than being
averaged twice over (an unweighted mean would report 0.30 where the
truth is 0.17); cohorts append in ingestion order rather than by hash
comparison; repository HEAD moving is not a measurement; the staleness
boundary is inclusive and is asserted from both sides and from a future
anchor; an unmeasured probe is not a zero failure rate and a cohort with
no denominator is neither; a promoted probe's statistic lives in
`history[-1]`; the semantic refusals write nothing at the incoming,
stored-cohort-on-ingest and stored-read boundaries alike; and the
summary is a pure reader, rendered through the CLI with the exact
commit.

Nothing here reads a clock: every case is written against the fixed
evaluation moment `support.NOW`, so a boundary case is a boundary case
on every machine and at every hour.
"""

from __future__ import annotations

import copy
import datetime
import json

from .support import (
    at, census_storage, census_summary, cli, cli_repo, COMMIT_A, COMMIT_B, DAY,
    expect, expect_refusal, measurement, NOW, probe_census, registry,
    result_document, scratch, seeded, summary_of, SYNTHETIC, unchanged,
)

import probe_engine  # type: ignore  # noqa: E402 -- `.support` installs tools/


# ==========================================================================
# Cohort semantics: the current statistic, its age, and staleness (#1429)
# ==========================================================================
COMMIT_C = "c" * 40


def test_cohort_accumulation() -> None:
    print("\n-- cohort accumulation and the combined statistic --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)

        # Two UNEQUAL batches on one commit. Averaging the stored batch
        # rates would give (0.5 + 0.1) / 2 = 0.30; the combined
        # numerator and denominator give 2 / 12 = 0.1667.
        census_storage.record_result(path, measurement(runs=2, failures=1,
                                                     age_days=2))
        census_storage.record_result(path, measurement(runs=10, failures=1,
                                                     age_days=1))
        summary = summary_of(path)
        expect(summary["requested_runs"] == 12
               and summary["failure_count"] == 2,
               "same-commit runs accumulate as summed counts")
        expect(abs(summary["failure_rate"] - 2 / 12) < 1e-12,
               "the rate is recomputed from the combined numerator and "
               "denominator, not averaged across batches")
        expect(abs(summary["failure_rate"] - 0.30) > 1e-9,
               "an unweighted mean of the two batch rates (0.30) is NOT "
               "what a cohort of unequal batches reports")
        expect(summary["sample_count"] == 2 and summary["cohort"] == "current",
               "both samples belong to one current cohort")

        # The freshness anchor is the LATEST contributing timestamp, so
        # an out-of-order same-commit result adds counts without
        # dragging it backwards.
        census_storage.record_result(path, measurement(runs=4, failures=0,
                                                     age_days=9))
        summary = summary_of(path)
        expect(summary["requested_runs"] == 16
               and summary["sample_count"] == 3,
               "an older same-commit result still contributes its counts")
        expect(summary["measured_at"] == at(1),
               "the freshness anchor is the latest contributing timestamp, "
               "never the most recently appended one")


def test_cohort_append_order() -> None:
    print("\n-- append order, not hash comparison --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        # A -> B -> A. Commit hashes have no intrinsic ordering, so the
        # third measurement opens a THIRD cohort rather than reopening
        # or merging with the first.
        census_storage.record_result(path, measurement(COMMIT_A, age_days=6))
        census_storage.record_result(path, measurement(COMMIT_B, age_days=4))
        census_storage.record_result(path, measurement(COMMIT_A, runs=5,
                                                     failures=5, age_days=2))
        census = json.loads(path.read_text(encoding="utf-8"))["probes"][0]["census"]
        expect([cohort["commit_sha"] for cohort in census["history"]]
               == [COMMIT_A, COMMIT_B],
               "A -> B -> A archives A then B, in the order they stopped "
               "being current")
        expect(census["current"]["commit_sha"] == COMMIT_A
               and len(census["current"]["samples"]) == 1,
               "the third measurement opens a NEW cohort for A, and does "
               "not reopen the first")
        summary = summary_of(path)
        expect(summary["requested_runs"] == 5 and summary["failure_count"] == 5
               and summary["failure_rate"] == 1.0,
               "the current statistic is the newest cohort alone, never "
               "pooled with the earlier cohort of the same commit")
        expect(sum(len(cohort["samples"])
                   for cohort in census["history"]) == 2,
               "every displaced cohort keeps its samples")


def test_head_movement_is_not_a_census_event() -> None:
    print("\n-- repository HEAD moving is not a measurement --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        census_storage.record_result(path, measurement(COMMIT_A, age_days=3))
        before_bytes = path.read_bytes()
        before = summary_of(path)

        # Nothing here records anything; the repository simply moved on.
        # Staleness is purely age-based, so the stored statistic and its
        # commit are exactly what they were.
        after = summary_of(path)
        unchanged(path, before_bytes,
                  "a HEAD change writes no census bytes")
        expect(after == before,
               "no commit movement can change a census summary; only a "
               "measurement does")
        expect(after["commit_sha"] == COMMIT_A,
               "the current statistic still names the commit it was "
               "measured on, not repository HEAD")

        # And the census never consults git for a summary: the same
        # document summarizes identically with the live registry's repo
        # root pointed somewhere else entirely.
        saved = probe_engine.REPO_ROOT
        probe_engine.REPO_ROOT = str(root)
        try:
            expect(summary_of(path) == before,
                   "a summary reads the stored cohort, never the working "
                   "tree it happens to run in")
        finally:
            probe_engine.REPO_ROOT = saved


def test_staleness_boundary() -> None:
    print("\n-- age and the staleness boundary --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        census_storage.record_result(path, measurement(age_days=7))
        summary = summary_of(path, stale_after_seconds=14 * DAY)
        expect(summary["age_seconds"] == 7 * DAY,
               "age is the distance from the anchor to the evaluation time")
        expect(summary["stale"] is False,
               "a cohort younger than the horizon is fresh")
        expect(summary_of(path, stale_after_seconds=7 * DAY)["stale"] is True,
               "the boundary is inclusive: age EQUAL to the horizon is stale")
        just_under = summary_of(path, stale_after_seconds=7 * DAY + 1)
        expect(just_under["stale"] is False,
               "one second more horizon than age is fresh")
        expect(summary_of(path, stale_after_seconds=0)["stale"] is True,
               "a zero horizon calls every measured cohort stale")

        # A cohort anchored in the FUTURE is the freshest thing there
        # is, never a negative age that would sort ahead of every real
        # measurement.
        future = summary_of(path, now=NOW - datetime.timedelta(days=10))
        expect(future["age_seconds"] == 0.0 and future["stale"] is False,
               "age is clamped at zero, so a future-anchored cohort is "
               "fresh rather than negatively old")


def test_unmeasured_and_zero_rate() -> None:
    print("\n-- unmeasured is not a zero failure rate --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        never = summary_of(path, "gamma")
        expect(never["measured"] is False,
               "a probe with no cohort reports measured: false")
        expect(all(never[field] is None for field in
                   ("cohort", "commit_sha", "measured_at", "age_seconds",
                    "stale", "sample_count", "requested_runs",
                    "failure_count", "failure_rate")),
               "every measurement field of an unmeasured probe is null")
        expect(never["failure_rate"] is None
               and not isinstance(never["failure_rate"], (int, float)),
               "an unmeasured probe never reports a zero failure rate")
        expect(never["key"] == "gamma"
               and never["classification"] == "manual-only",
               "it still carries its inventory identity")

        census_storage.record_result(
            path, measurement(probe="gamma", runs=8, failures=0, age_days=1))
        clean = summary_of(path, "gamma")
        expect(clean["measured"] is True and clean["failure_rate"] == 0.0
               and clean["requested_runs"] == 8,
               "a probe measured eight times with no failure reports a real "
               "rate of 0.0")
        expect(clean["stale"] is False,
               "and a real zero rate is still classified for freshness")

        # A cohort with no denominator has no rate at all. Reporting
        # 0.0 there would spell "never failed" for runs nobody made.
        no_runs = measurement(probe="beta", commit=COMMIT_B, runs=0,
                              failures=0, age_days=1)
        no_runs["runs"] = []
        # No run completed, so every DECLARED check tallies zero — the
        # shape `Measurement.check_counts()` starts from and never
        # leaves when the loop appends nothing. An empty map would be a
        # different claim (a probe declaring no checks at all), which
        # #1493's keying rule refuses; the denominator this case is
        # about is `requested_runs`, which is untouched either way.
        no_runs["check_counts"] = {check["id"]: {"PASS": 0, "FAIL": 0,
                                                 "MISSING": 0}
                                   for check in no_runs["checks"]}
        census_storage.record_result(path, no_runs)
        empty = summary_of(path, "beta")
        expect(empty["measured"] is True and empty["requested_runs"] == 0
               and empty["failure_rate"] is None,
               "a cohort that requested no runs reports a null rate, not 0.0")


def test_history_only_statistic() -> None:
    print("\n-- a promoted probe's statistic lives in history --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        census_storage.record_result(path, measurement(COMMIT_A, runs=3,
                                                     failures=3, age_days=30))
        census_storage.record_result(path, measurement(COMMIT_B, runs=4,
                                                     failures=1, age_days=9))
        # Promotion archives the current cohort and does not restore it
        # on a later downgrade, so `current` is null while the newest
        # measured statistic is real and must still be reported.
        with registry(ci_eligible=("alpha",)):
            census_storage.ensure_document(path)
        census = json.loads(path.read_text(encoding="utf-8"))["probes"][0]["census"]
        expect(census["current"] is None and len(census["history"]) == 2,
               "promotion archives the current cohort, keeping both")

        summary = summary_of(path, stale_after_seconds=9 * DAY)
        expect(summary["measured"] is True and summary["cohort"] == "history",
               "a history-only record is MEASURED, summarized from the last "
               "archived cohort")
        expect(summary["commit_sha"] == COMMIT_B
               and summary["requested_runs"] == 4
               and summary["failure_count"] == 1,
               "the statistic comes from history[-1], not from the older "
               "cohort and not from both pooled")
        expect(summary["age_seconds"] == 9 * DAY and summary["stale"] is True,
               "its freshness is its own latest sample's age at the "
               "boundary, unaffected by having been archived")
        lenient = summary_of(path, stale_after_seconds=10 * DAY)
        expect(lenient["stale"] is False,
               "and archiving alone never makes a record stale")

        # A downgrade refreshes the classification and nothing else, so
        # the archived statistic remains the authoritative one.
        census_storage.ensure_document(path)
        after = summary_of(path, stale_after_seconds=9 * DAY)
        expect(after["cohort"] == "history"
               and after["commit_sha"] == COMMIT_B,
               "a downgrade restores no cohort, so history[-1] stays "
               "authoritative")


def test_cohort_semantic_refusals() -> None:
    print("\n-- unusable semantic values refuse, writing nothing --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        census_storage.record_result(path, measurement(age_days=1))
        before = path.read_bytes()

        # The placeholder `probe_flake` writes when git could not be
        # consulted is well-formed and schema-valid; it names no commit,
        # so it may not open or extend a cohort.
        expect_refusal(
            lambda: census_storage.record_result(
                path, measurement(commit="unknown", age_days=0)),
            "the `unknown` commit placeholder cannot open a cohort",
            "unknown", "no commit")
        unchanged(path, before, "and that refusal wrote nothing")

        for commit, why in (("a" * 39, "an abbreviated hash"),
                            ("A" * 40, "an uppercase hash"),
                            ("z" * 40, "a non-hex hash")):
            expect_refusal(
                lambda commit=commit: census_storage.record_result(
                    path, measurement(commit=commit)),
                f"{why} is refused as a cohort identity",
                "lowercase hex")
        unchanged(path, before, "and none of those wrote anything")

        # A harness error is deliberately NOT gated: it contributes to
        # no cohort, and unmeasurable provenance is exactly what the
        # attempt log retains.
        census_storage.record_result(path, result_document(
            status="harness-error", commit="unknown"))
        census = json.loads(path.read_text(encoding="utf-8"))["probes"][0]["census"]
        expect(census["attempts"][-1]["commit_sha"] == "unknown"
               and census["attempts"][-1]["accepted"] is False,
               "a harness error with unknown provenance is still logged")
        expect(len(census["current"]["samples"]) == 1
               and census["history"] == [],
               "and it opened no cohort")

        # The same checks fail closed over ALREADY-STORED state, which
        # is how a census written before them (or by hand) is caught.
        document = json.loads(path.read_text(encoding="utf-8"))
        document["probes"][0]["census"]["current"]["commit_sha"] = "unknown"
        expect_refusal(
            lambda: census_summary.census_summary(
                document, now=NOW, stale_after_seconds=DAY, probe="alpha"),
            "a stored cohort keyed by the placeholder refuses on READ",
            "unknown")

        stored = json.loads(path.read_text(encoding="utf-8"))
        stored["probes"][0]["census"]["current"]["samples"][0][
            "commit_sha"] = "unknown"
        expect_refusal(
            lambda: census_summary.census_summary(
                stored, now=NOW, stale_after_seconds=DAY, probe="alpha"),
            "a stored SAMPLE carrying the placeholder refuses on READ too",
            "sample 0", "unknown")

        stored = json.loads(path.read_text(encoding="utf-8"))
        stored["probes"][0]["census"]["current"]["samples"][0][
            "timestamp_utc"] = "2026-08-21 05:00:00"
        expect_refusal(
            lambda: census_summary.census_summary(
                stored, now=NOW, stale_after_seconds=DAY, probe="alpha"),
            "a stored sample whose timestamp cannot be read refuses on READ",
            "timestamp")

        stored = json.loads(path.read_text(encoding="utf-8"))
        stored["probes"][0]["census"]["current"]["samples"][0][
            "requested_runs"] = -3
        expect_refusal(
            lambda: census_summary.census_summary(
                stored, now=NOW, stale_after_seconds=DAY, probe="alpha"),
            "a stored negative run count refuses on READ",
            "negative")

        stored = json.loads(path.read_text(encoding="utf-8"))
        stored["probes"][0]["census"]["current"]["samples"] = []
        expect_refusal(
            lambda: census_summary.census_summary(
                stored, now=NOW, stale_after_seconds=DAY, probe="alpha"),
            "a stored cohort with no samples has no statistic and refuses",
            "no samples")

        # The append-or-archive decision READS the stored cohort, so an
        # unusable one refuses the whole ingestion rather than being
        # quietly extended (same commit) or archived into history
        # (different commit). Before this, a valid measurement landed
        # and only the later READ failed.
        # `unknown` is spelled on the cohort AND its samples, which is
        # what a census written when `git` could not be consulted really
        # looks like: `ingest_result` copies one commit into both. It
        # also keeps the cohort internally consistent, so #1493's
        # membership rule stays silent and this case really exercises
        # #1429's identity check rather than being pre-empted by it.
        # The second case is the reverse on purpose: a sample that
        # disagrees with its cohort is #1493's, and it is reached first,
        # which the fragments below pin so neither rule can quietly
        # stop covering its own shape.
        for damage, why, fragment in (
                (lambda c: c.update(
                    {"commit_sha": "unknown",
                     "samples": [{**sample, "commit_sha": "unknown"}
                                 for sample in c["samples"]]}),
                 "keyed by the placeholder", "unknown"),
                (lambda c: c["samples"][0].update({"commit_sha": "unknown"}),
                 "holding a placeholder sample",
                 "a cohort holds one commit's samples"),
                (lambda c: c["samples"][0].update(
                    {"timestamp_utc": "2026-08-21 05:00:00"}),
                 "holding an unreadable sample timestamp", "timestamp"),
                (lambda c: c["samples"][0].update({"requested_runs": -1}),
                 "holding a negative sample run count", "requested_runs")):
            with scratch() as damaged_root:
                damaged_path = damaged_root / "probe_census.json"
                seeded(damaged_path)
                census_storage.record_result(damaged_path,
                                           measurement(COMMIT_A, age_days=3))
                stored = json.loads(damaged_path.read_text(encoding="utf-8"))
                damage(stored["probes"][0]["census"]["current"])
                damaged_path.write_text(json.dumps(stored), encoding="utf-8")
                damaged_before = damaged_path.read_bytes()
                for follow_up, kind in ((COMMIT_A, "same-commit"),
                                        (COMMIT_B, "different-commit")):
                    expect_refusal(
                        lambda follow_up=follow_up:
                            census_storage.record_result(
                                damaged_path,
                                measurement(follow_up, age_days=0)),
                        f"a {kind} measurement onto a stored cohort {why} "
                        f"refuses", fragment)
                    unchanged(damaged_path, damaged_before,
                              f"...and that {kind} refusal wrote nothing")

        # A harness error never reads the current cohort, so a damaged
        # one does not stop the attempt log from recording the failure.
        with scratch() as damaged_root:
            damaged_path = damaged_root / "probe_census.json"
            seeded(damaged_path)
            census_storage.record_result(damaged_path,
                                       measurement(COMMIT_A, age_days=3))
            stored = json.loads(damaged_path.read_text(encoding="utf-8"))
            # Damaged the way a real placeholder-provenance census is —
            # cohort and samples alike — so this pins #1429's "a harness
            # error never reads the cohort" and not an incidental
            # #1493 membership violation the whole census would stop on.
            cohort = stored["probes"][0]["census"]["current"]
            cohort["commit_sha"] = "unknown"
            for sample in cohort["samples"]:
                sample["commit_sha"] = "unknown"
            damaged_path.write_text(json.dumps(stored), encoding="utf-8")
            census_storage.record_result(damaged_path, result_document(
                status="harness-error", commit=COMMIT_B))
            after = json.loads(
                damaged_path.read_text(encoding="utf-8"))["probes"][0]["census"]
            expect(len(after["attempts"]) == 2
                   and after["attempts"][-1]["accepted"] is False,
                   "a harness error still logs against a damaged stored "
                   "cohort, which it never reads")

        # The evaluation time and the horizon are inputs, and an
        # unusable one is a refusal rather than a substituted default.
        good = json.loads(path.read_text(encoding="utf-8"))
        expect_refusal(
            lambda: census_summary.census_summary(
                good, now=datetime.datetime(2026, 8, 21),
                stale_after_seconds=DAY),
            "a naive evaluation time is refused, never assumed to be UTC",
            "timezone-aware")
        expect_refusal(
            lambda: census_summary.census_summary(
                good, now=NOW, stale_after_seconds=-1),
            "a negative staleness horizon is refused",
            "nonnegative")
        expect_refusal(
            lambda: census_summary.census_summary(
                good, now=NOW, stale_after_seconds=float("inf")),
            "a non-finite staleness horizon is refused",
            "finite")
        expect_refusal(
            lambda: census_summary.census_summary(
                good, now=NOW, stale_after_seconds=DAY, probe="nonesuch"),
            "summarizing a probe with no census row refuses",
            "no census row")


def test_summary_preserves_everything() -> None:
    print("\n-- the summary is a pure reader --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        census_storage.record_result(path, measurement(COMMIT_A, age_days=20))
        census_storage.record_result(path, measurement(COMMIT_B, age_days=10))
        census_storage.record_result(path, measurement(COMMIT_C, age_days=1))
        before_bytes = path.read_bytes()
        document = json.loads(path.read_text(encoding="utf-8"))
        original = copy.deepcopy(document)

        summaries = census_summary.census_summary(
            document, now=NOW, stale_after_seconds=14 * DAY)
        expect(document == original,
               "summarizing mutates no part of the document it reads")
        unchanged(path, before_bytes, "and writes no bytes")
        expect(len(summaries) == len(SYNTHETIC)
               and [s["key"] for s in summaries]
               == [key for key, _script, _purpose in SYNTHETIC],
               "the whole-census view reports every row, in inventory order")

        census = document["probes"][0]["census"]
        expect([cohort["commit_sha"] for cohort in census["history"]]
               == [COMMIT_A, COMMIT_B]
               and census["current"]["commit_sha"] == COMMIT_C,
               "every displaced cohort is still retained after two "
               "displacements")
        expect(summaries[0]["commit_sha"] == COMMIT_C,
               "and the newest one is the authoritative statistic")


def test_summary_cli() -> None:
    print("\n-- the --summary CLI --")
    with registry(), cli_repo() as (_main_wt, census_path):
        cli("--seed")
        census_storage.record_result(census_path,
                                   measurement(runs=4, failures=1, age_days=8))

        code, out, err = cli("--summary", "--as-of", at(0),
                             "--stale-after-days", "7", "--json")
        expect(code == 0 and err == "", f"--summary --json exits 0 ({err!r})")
        payload = json.loads(out)
        alpha = next(row for row in payload if row["key"] == "alpha")
        expect(alpha["stale"] is True and alpha["age_seconds"] == 8 * DAY
               and alpha["requested_runs"] == 4,
               "--json reports the combined statistic and the injected age")
        expect(any(row["measured"] is False for row in payload),
               "and reports unmeasured probes as such")

        code, out, _ = cli("--summary", "--as-of", at(0),
                           "--stale-after-days", "30", "--probe", "alpha",
                           "--json")
        single = json.loads(out)
        expect(code == 0 and len(single) == 1 and single[0]["key"] == "alpha"
               and single[0]["stale"] is False,
               "--probe narrows to one row, and the horizon is honoured")

        code, out, _ = cli("--summary", "--as-of", at(0))
        expect(code == 0 and "alpha" in out and "unmeasured" in out
               and "%" in out,
               "the default rendering is a human table")
        expect(COMMIT_A in out,
               "the table reports the EXACT commit, not an abbreviation")
        expect(all(COMMIT_A[:8] not in line or COMMIT_A in line
                   for line in out.splitlines()),
               "and no row abbreviates it")

        # The evaluation time and horizon are validated like every other
        # input, and the three new flags belong to --summary alone.
        code, _, err = cli("--summary", "--as-of", "yesterday")
        expect(code == 1 and "--as-of" in err,
               "an unreadable --as-of refuses instead of falling back to now")
        code, _, err = cli("--summary", "--stale-after-days", "soon")
        expect(code == 1 and "--stale-after-days" in err,
               "a non-numeric --stale-after-days refuses")
        code, _, err = cli("--summary", "--stale-after-days", "none")
        expect(code == 1 and "none" in err,
               "there is no `none` horizon")
        for argv, flag in ((("--print", "--json"), "--json"),
                           (("--print", "--as-of", at(0)), "--as-of"),
                           (("--validate", "--stale-after-days", "3"),
                            "--stale-after-days")):
            code, _, err = cli(*argv)
            expect(code == 1 and flag in err,
                   f"{flag} outside --summary is reported, not ignored")
        code, _, err = cli("--summary", "--probe", "nonesuch")
        expect(code == 1 and "nonesuch" in err,
               "--summary --probe on an unknown key refuses")


#: This family's complete ordered inventory, and the whole of its
#: contribution to the aggregate, which runs it last of the five owners
#: in this package.
TESTS = (
    test_cohort_accumulation,
    test_cohort_append_order,
    test_head_movement_is_not_a_census_event,
    test_staleness_boundary,
    test_unmeasured_and_zero_rate,
    test_history_only_statistic,
    test_cohort_semantic_refusals,
    test_summary_preserves_everything,
    test_summary_cli,
)
