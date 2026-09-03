#!/usr/bin/env python3
"""The declared schema and the cross-field invariants (#2129).

Five groups, in the aggregate's order:

  `test_adversarial_malformed_input`  the fuzz sweep over both input
                                      surfaces -- every field retyped to
                                      each JSON shape it could be
                                      confused with, plus removal, plus
                                      the producer-only fields that must
                                      never ride into an accepted census;
  `test_cross_field_invariants`       #1493's rules, each with a
                                      rejecting fixture that is still
                                      SCHEMA-valid, the legitimate flows
                                      it must not over-reject, and a
                                      mutation lifting that one rule out
                                      of the production rule set;
  `test_declared_schema`              #1492's checked-in
                                      `probe_census_schema.json`, driven
                                      against a valid census and result
                                      and against every deletion, retype
                                      and out-of-range value it declares;
  `test_malformed_schema_file`        a broken schema file refusing every
                                      operation cleanly;
  `test_missing_dependency`           an environment where `jsonschema`
                                      is genuinely unimportable.

This is the slowest family by an order of magnitude -- the exhaustive
schema surface dominates the aggregate's runtime -- which is most of why
`--family` exists: the other five each finish in well under a second.
"""

from __future__ import annotations

import itertools
import json
import shutil
import sys
import tempfile
from contextlib import contextmanager
from pathlib import Path

from .support import (
    _alpha, census_contract, census_records, census_storage, cli, cli_repo,
    COMMIT_A, COMMIT_B, expect, expect_refusal, probe_census, registry,
    result_document, rich_census, scratch, seeded, stored_v3_document,
    stored_v4_document, v1_document,
)

import probe_flake  # type: ignore  # noqa: E402 -- `.support` installs tools/


DELETE = object()


# Retyped to every JSON shape the field could plausibly be confused
# with, plus removal. `inf`/`nan` are here because `json.loads` accepts
# Python's non-standard spellings, so a census really can hold one.
FUZZ_VALUES = (None, 0, 5, -1, 1.5, "", "x", True, [], {}, [5], {"a": 1},
               [[]], float("inf"), float("nan"), DELETE)


# Producer-only fields `Measurement.to_document` adds. None may ride
# into an ACCEPTED census, whatever the rest of the document looks like.
PRODUCER_ONLY = ("port", "error_run", "artifact_root", "invocation_dir",
                 "label")


def _locations(node, prefix=()):
    """Every addressable location in a JSON document, depth first."""
    if isinstance(node, dict):
        for key, value in node.items():
            yield prefix + (key,)
            yield from _locations(value, prefix + (key,))
    elif isinstance(node, list):
        for index, value in enumerate(node):
            yield prefix + (index,)
            yield from _locations(value, prefix + (index,))


def _replace(document, path, value):
    """Set (or DELETE) one location. False when the path is unreachable."""
    node = document
    try:
        for step in path[:-1]:
            node = node[step]
        if value is DELETE:
            if isinstance(node, dict):
                node.pop(path[-1], None)
            else:
                node.pop(path[-1])
        else:
            node[path[-1]] = value
    except (KeyError, IndexError, TypeError, AttributeError):
        return False
    return True


def stored_v2_document() -> dict:
    """A `probe-census/v2` census exactly as #1428 wrote one.

    Six-field records, no `claims`. This is what `--seed` migrates FROM,
    so it is spelled out here rather than derived from the current
    `empty_census()`: deriving it would silently start testing the
    current shape the moment the record grows another field.
    """
    return {
        "schema": "probe-census/v2",
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
            },
        }],
    }


def test_adversarial_malformed_input() -> None:
    """Retype and delete EVERY field of both input surfaces (#1503).

    Five review rounds each surfaced one more parseable-but-malformed
    input that crashed an operation or half-wrote through it, one field
    at a time. This sweep is what closes the class rather than the
    instance: it drives every operation over every location of a stored
    census and of a result document, and holds all three promises at
    once — no uncontrolled exception, no byte changed by a refusal, and
    no producer-only field in an accepted record.

    It is exhaustive rather than random, so it is fully deterministic:
    the locations come from insertion-ordered dicts and the values from
    a fixed tuple. It deliberately does not enumerate WHICH mutations
    must be rejected — that is the declared schema's specification, and
    restating it case by case here would rebuild by hand exactly the
    surface #1492 replaced. What it asserts is the three promises that
    hold whatever the schema says, and that both outcomes really occur.
    """
    print("\n-- adversarial sweep over both input surfaces --")
    with registry(ci_eligible={"beta"}), scratch() as root:
        target = root / "probe_census.json"

        operations = (
            ("--seed", lambda p: census_storage.ensure_document(p)),
            ("--record", lambda p: census_storage.record_result(
                p, result_document(commit=COMMIT_B))),
            ("--set-acceptable-failures", lambda p: census_storage.record_policy(
                p, "alpha", acceptable_failures=1, justification="j")),
            ("--set-estimate", lambda p: census_storage.record_policy(
                p, "alpha", estimate=9)),
            ("--validate", lambda p: census_records.validate_manifest(
                census_storage.load(p))),
        )

        uncontrolled: list[str] = []
        disturbed: list[str] = []
        runs = refused = accepted = 0
        locations = list(_locations(rich_census()))
        for path, value in itertools.product(locations, FUZZ_VALUES):
            document = rich_census()
            if not _replace(document, path, value):
                continue
            try:
                text = json.dumps(document)
            except (TypeError, ValueError):
                continue          # not representable, so not reachable
            where = ".".join(str(step) for step in path)
            for name, operation in operations:
                target.write_text(text, encoding="utf-8")
                stored = target.read_bytes()
                runs += 1
                try:
                    operation(target)
                except (census_contract.CensusError,
                        census_storage.DocsWorktreeMissing):
                    refused += 1
                    if target.read_bytes() != stored:
                        disturbed.append(f"{name} on {where}={value!r}")
                except BaseException as error:  # noqa: BLE001 - the bug IS this
                    uncontrolled.append(
                        f"{name} on {where}={value!r} raised "
                        f"{type(error).__name__}: {error}")
                else:
                    accepted += 1

        expect(runs > 5000,
               f"the census sweep really ran ({runs} operations over "
               f"{len(locations)} locations)")
        expect(refused > 0 and accepted > 0,
               f"it exercised both outcomes ({refused} refused, "
               f"{accepted} accepted), so it cannot pass vacuously")
        expect(uncontrolled == [],
               f"no malformed census produces an uncontrolled exception "
               f"({len(uncontrolled)}: {uncontrolled[:3]})")
        expect(disturbed == [],
               f"no refusal changes a byte of the census "
               f"({len(disturbed)}: {disturbed[:3]})")

        # -- the other surface: the result document `--record` consumes --
        # The census sweep leaves its last mutated document on disk, and
        # the declared schema will not reconcile that into a fresh one;
        # the second sweep starts from a newly seeded census.
        target.unlink()
        census_storage.ensure_document(target)
        clean = target.read_bytes()
        uncontrolled, disturbed, leaked = [], [], []
        runs = refused = accepted = 0
        for path, value in itertools.product(
                list(_locations(result_document())), FUZZ_VALUES):
            document = result_document()
            if not _replace(document, path, value):
                continue
            try:
                json.dumps(document, allow_nan=False)
            except (TypeError, ValueError):
                continue
            where = ".".join(str(step) for step in path)
            target.write_bytes(clean)
            runs += 1
            try:
                census_storage.record_result(target, document)
            except (census_contract.CensusError,
                    census_storage.DocsWorktreeMissing):
                refused += 1
                if target.read_bytes() != clean:
                    disturbed.append(f"{where}={value!r}")
            except BaseException as error:  # noqa: BLE001
                uncontrolled.append(f"{where}={value!r} raised "
                                    f"{type(error).__name__}: {error}")
            else:
                accepted += 1
                text = target.read_text(encoding="utf-8")
                leaked += [f"{name!r} via {where}={value!r}"
                           for name in PRODUCER_ONLY if f'"{name}"' in text]

        expect(runs > 300,
               f"the result sweep really ran ({runs} --record operations)")
        expect(refused > 0 and accepted > 0,
               f"it exercised both outcomes ({refused} refused, "
               f"{accepted} accepted)")
        expect(uncontrolled == [],
               f"no malformed result produces an uncontrolled exception "
               f"({len(uncontrolled)}: {uncontrolled[:3]})")
        expect(disturbed == [],
               f"no refused result changes a byte of the census "
               f"({len(disturbed)}: {disturbed[:3]})")
        expect(leaked == [],
               f"no accepted result carries a producer-only field into the "
               f"census ({len(leaked)}: {leaked[:3]})")


# ==========================================================================
# The declared schema (#1492)
# ==========================================================================
def expect_valid(call, msg: str) -> None:
    """`call` accepts. A refusal is the failure, reported not raised."""
    try:
        call()
    except Exception as error:  # noqa: BLE001 - a refusal here IS the bug
        expect(False, f"{msg} (refused: {type(error).__name__}: {error})")
        return
    expect(True, msg)


def harness_error_result() -> dict:
    """A well-formed harness error, carrying the run that broke."""
    return result_document(status="harness-error")


def _no_runs_result() -> dict:
    """A harness error on the very FIRST run, so nothing completed.

    `measure` creates the run directory, launches, and returns through
    `stop_with_harness_error` before anything joins `runs` — so
    `check_counts` is the descriptor's ids seeded to all zeros, exactly
    what `Measurement.check_counts()` starts from.
    """
    return result_document(
        status="harness-error", error="run 1: unreadable event stream",
        requested_runs=3, completed_runs=0, runs=[],
        error_run={"index": 1, "port": 9100, "outcome": "HARNESS_ERROR",
                   "elapsed_seconds": 0.5, "checks": {},
                   "artifact_dir": "/tmp/artifacts/run-001"},
        check_counts={"first": {"PASS": 0, "FAIL": 0, "MISSING": 0},
                      "second": {"PASS": 0, "FAIL": 0, "MISSING": 0}},
        worst_elapsed_seconds=0.0, total_elapsed_seconds=0.0,
        retained_artifacts=["/tmp/artifacts/run-001"])


def test_declared_schema() -> None:
    """The schema file itself, and what it now refuses (#1492).

    Every rejection is driven through a REAL operation against a real
    census on disk, so each case proves the two things a declared
    validator has to prove together: the document is refused, and the
    authoritative bytes are not touched by the refusal.
    """
    print("\n-- the declared JSON Schema --")
    schema = census_contract.load_schema()
    expect(schema.get("$schema") == "https://json-schema.org/draft/2020-12/schema",
           "the checked-in schema identifies a supported draft")
    # `load_schema` runs that draft's own meta-schema check, so reaching
    # here at all is the self-check passing.
    expect(set(census_contract.SCHEMA_DEFINITIONS)
           == {census_contract.SEED_SCHEMA, census_contract.RECORD_SCHEMA,
               census_contract.CLAIM_SCHEMA, census_contract.OUTCOME_SCHEMA,
               census_contract.CENSUS_SCHEMA,
               census_contract.RESULT_SCHEMA},
           "every document kind the tool reads has a declared schema")
    expect(all(name in (schema.get("$defs") or {})
               for name in census_contract.SCHEMA_DEFINITIONS.values()),
           "each declared schema names a definition the file really has")
    expect(all("/" not in name
               for name in census_contract.SCHEMA_DEFINITIONS.values()),
           "no definition name contains a JSON Pointer separator")
    expect_refusal(
        lambda: census_contract.validate_document({}, "probe-census/v9", "x"),
        "asking for an undeclared schema is a controlled refusal",
        "probe-census/v9")

    with registry(ci_eligible={"beta"}), scratch() as root:
        # Applying every declared schema to a document that satisfies it
        # is what proves the file's internal `$ref`s all resolve — a
        # schema that self-checks can still be unusable.
        expect_valid(lambda: census_contract.validate_document(
            v1_document(), census_contract.SEED_SCHEMA, "a v1 seed"),
            "the v1 seed schema accepts a real v1 seed")
        expect_valid(lambda: census_contract.validate_document(
            rich_census(), census_contract.CENSUS_SCHEMA, "a v5 census"),
            "the v5 census schema accepts a real measured census")
        expect_valid(lambda: census_contract.validate_document(
            stored_v2_document(), census_contract.RECORD_SCHEMA, "a v2 census"),
            "the FROZEN v2 schema still accepts a real stored v2 census, "
            "which is what --seed migrates from")
        expect_valid(lambda: census_contract.validate_document(
            stored_v3_document(), census_contract.CLAIM_SCHEMA, "a v3 census"),
            "the FROZEN v3 schema still accepts a real stored v3 census, "
            "which --seed also migrates from")
        expect_valid(lambda: census_contract.validate_document(
            stored_v4_document(), census_contract.OUTCOME_SCHEMA, "a v4 census"),
            "the FROZEN v4 schema still accepts a real stored v4 census, "
            "which --seed also migrates from")
        expect_valid(lambda: census_contract.validate_document(
            census_records.build_manifest(), census_contract.CENSUS_SCHEMA,
            "a fresh manifest"),
            "...and the manifest this tool generates for itself")
        expect_valid(lambda: census_contract.validate_result(result_document()),
                     "the result schema accepts a real ok measurement")
        expect_valid(
            lambda: census_contract.validate_result(harness_error_result()),
            "...and a harness error carrying its HARNESS_ERROR run")

        target = root / "probe_census.json"
        clean = root / "clean.json"
        census_storage.ensure_document(clean)
        clean_bytes = clean.read_bytes()

        def refuses_census(mutate, fragment, why) -> None:
            """A stored census `mutate` breaks: refused, and unchanged."""
            document = rich_census()
            mutate(document)
            target.write_text(json.dumps(document), encoding="utf-8")
            stored = target.read_bytes()
            expect_refusal(
                lambda: census_storage.record_result(target, result_document()),
                f"a census with {why} is refused", fragment)
            expect(target.read_bytes() == stored,
                   f"...and the refusal changed no bytes ({why})")

        def refuses_result(mutate, fragment, why) -> None:
            """A result document `mutate` breaks: refused, nothing written."""
            document = result_document()
            mutate(document)
            expect_refusal(
                lambda: census_storage.record_result(clean, document),
                f"a result with {why} is refused", fragment)
            expect(clean.read_bytes() == clean_bytes,
                   f"...and the refusal wrote nothing ({why})")

        # -- a nullable field DELETED, not set to null (`6a23027f`) -----
        # Nullable is spelled as a REQUIRED null-inclusive type, so
        # removing one is a violation rather than an absence. That is
        # the whole difference between `additionalProperties`/`required`
        # and reading fields with `.get()`.
        for field in census_records.empty_census():
            refuses_census(
                lambda d, f=field: d["probes"][0]["census"].pop(f),
                f"'{field}' is a required property",
                f"the census field `{field}` deleted")
        for field in ("key", "script", "classification", "protocol", "census"):
            refuses_census(
                lambda d, f=field: d["probes"][0].pop(f),
                f"'{field}' is a required property",
                f"the inventory field `{field}` deleted")
        for field in ("timestamp_utc", "commit_sha", "failure_rate",
                      "retained_artifacts", "check_counts"):
            refuses_census(
                lambda d, f=field: d["probes"][0]["census"]["current"][
                    "samples"][0].pop(f),
                f"'{field}' is a required property",
                f"a stored sample missing `{field}`")

        # -- a truthy non-object `runs[i].checks` (`6ddc01d9`) ----------
        # This one used to raise AttributeError from inside the
        # transaction, because a truthy value passed the guard and then
        # was asked for `.items()`.
        for value, why in ((5, "the number 5"), ("PASS", "a string"),
                           (["first"], "a non-empty list"),
                           (True, "the boolean True"), (0, "the number 0"),
                           (None, "null"), ([], "an empty list")):
            refuses_result(
                lambda d, v=value: d["runs"][0].__setitem__("checks", v),
                "$.runs[0].checks", f"a per-run `checks` that is {why}")
        refuses_result(
            lambda d: d["runs"][0]["checks"].__setitem__("first", "MAYBE"),
            "$.runs[0].checks.first", "an unrecognized per-run check result")

        # -- unexpected properties, in every representative object -----
        for mutate, fragment, why in (
            (lambda d: d.__setitem__("extra", 1), "$", "the result root"),
            (lambda d: d["runs"][0].__setitem__("extra", 1), "$.runs[0]",
             "a result run"),
            (lambda d: d["checks"][0].__setitem__("extra", 1), "$.checks[0]",
             "a check descriptor"),
            (lambda d: d["check_counts"]["first"].__setitem__("SKIPPED", 1),
             "$.check_counts.first", "a check tally"),
        ):
            refuses_result(mutate, fragment,
                           f"an unexpected property in {why}")
        for mutate, fragment, why in (
            (lambda d: d.__setitem__("extra", 1), "$", "the census root"),
            (lambda d: d["probes"][0].__setitem__("extra", 1), "$.probes[0]",
             "an inventory row"),
            (lambda d: d["probes"][0]["census"].__setitem__("extra", 1),
             "$.probes[0].census", "a census record"),
            (lambda d: d["probes"][0]["census"]["current"].__setitem__(
                "extra", 1), "$.probes[0].census.current", "a cohort"),
            (lambda d: d["probes"][0]["census"]["current"]["samples"][
                0].__setitem__("port", 9100),
             "$.probes[0].census.current.samples[0]",
             "a stored sample (a producer-only field)"),
            (lambda d: d["probes"][0]["census"]["attempts"][0].__setitem__(
                "extra", 1), "$.probes[0].census.attempts[0]", "an attempt"),
        ):
            refuses_census(mutate, fragment,
                           f"an unexpected property in {why}")

        # -- enums, ranges and lengths ---------------------------------
        for mutate, fragment, why in (
            (lambda d: d["probes"][0].__setitem__("classification", "maybe"),
             "$.probes[0].classification", "an unrecognized classification"),
            (lambda d: d["probes"][0].__setitem__("protocol", "probe/v9"),
             "$.probes[0].protocol", "an unrecognized protocol status"),
            (lambda d: d["probes"][0]["census"].__setitem__(
                "acceptable_failures", -1),
             "$.probes[0].census.acceptable_failures", "a negative X"),
            (lambda d: d["probes"][0]["census"].__setitem__(
                "acceptable_failures", 10),
             "$.probes[0].census.acceptable_failures",
             "an X of 10, which would accept a probe that never passes"),
            (lambda d: d["probes"][0]["census"].__setitem__(
                "acceptable_failures", True),
             "$.probes[0].census.acceptable_failures",
             "a boolean X (`bool` is an `int` subclass, so this needs its "
             "own rejection)"),
            (lambda d: d["probes"][0]["census"].__setitem__(
                "estimated_worst_case_seconds", -1),
             "$.probes[0].census.estimated_worst_case_seconds",
             "a negative estimate"),
            (lambda d: d["probes"][0]["census"].__setitem__(
                "acceptable_failures_justification", "x" * 4001),
             "$.probes[0].census.acceptable_failures_justification",
             "a justification past its length bound"),
            (lambda d: d["probes"][0]["census"]["current"].__setitem__(
                "commit_sha", ""),
             "$.probes[0].census.current.commit_sha", "an empty commit sha"),
            (lambda d: d["probes"][0]["census"]["current"]["samples"][
                0].__setitem__("failure_rate", 1.5),
             "$.probes[0].census.current.samples[0].failure_rate",
             "a failure rate above 1"),
            (lambda d: d["probes"][0]["census"]["current"]["samples"][
                0].__setitem__("timestamp_utc", "yesterday"),
             "$.probes[0].census.current.samples[0].timestamp_utc",
             "a timestamp in no recognized form"),
            (lambda d: d["probes"][0]["census"]["current"]["samples"][0][
                "runs"][0].__setitem__("outcome", "HARNESS_ERROR"),
             "$.probes[0].census.current.samples[0].runs[0].outcome",
             "HARNESS_ERROR as a stored run outcome, which it never is"),
            (lambda d: d["probes"][0]["census"]["attempts"][0].__setitem__(
                "status", "nope"),
             "$.probes[0].census.attempts[0].status",
             "an unrecognized attempt status"),
            (lambda d: d["probes"][0]["census"]["attempts"][0].__setitem__(
                "accepted", "yes"),
             "$.probes[0].census.attempts[0].accepted",
             "a non-boolean `accepted`"),
        ):
            refuses_census(mutate, fragment, why)
        for mutate, fragment, why in (
            (lambda d: d["runs"][0].__setitem__("port", 0), "$.runs[0].port",
             "a port below the representable range"),
            (lambda d: d["runs"][0].__setitem__("port", 70000),
             "$.runs[0].port", "a port above the representable range"),
            (lambda d: d.__setitem__("rts_capabilities", 0),
             "$.rts_capabilities", "zero RTS capabilities"),
            (lambda d: d.__setitem__("failure_count", -1), "$.failure_count",
             "a negative failure count"),
            (lambda d: d.__setitem__("worst_elapsed_seconds", -1),
             "$.worst_elapsed_seconds", "a negative elapsed time"),
            (lambda d: d["retained_artifacts"].append(""),
             "$.retained_artifacts[1]", "an empty artifact path"),
            (lambda d: d.__setitem__("commit_sha", "c" * 65), "$.commit_sha",
             "a commit sha past its length bound"),
        ):
            refuses_result(mutate, fragment, why)

        # -- non-finite numbers, which no schema bound can express -----
        # `json.loads` really does accept these spellings, and `maximum`
        # does not reject a NaN: no comparison with one is ever true.
        for value, why in ((float("nan"), "NaN"), (float("inf"), "Infinity"),
                           (float("-inf"), "-Infinity")):
            refuses_census(
                lambda d, v=value: d["probes"][0]["census"].__setitem__(
                    "estimated_worst_case_seconds", v),
                "$.probes[0].census.estimated_worst_case_seconds",
                f"a stored {why}")
            refuses_result(
                lambda d, v=value: d.__setitem__("total_elapsed_seconds", v),
                "$.total_elapsed_seconds", f"an incoming {why}")
        expect_refusal(
            lambda: census_storage.record_policy(
                clean, "alpha", estimate=float("nan")),
            "a policy update may not store a NaN either",
            "non-finite")
        expect(clean.read_bytes() == clean_bytes,
               "...and that refusal wrote nothing")

        # -- the schema DISCRIMINATOR, which is not a schema keyword ----
        target.write_text(json.dumps({**rich_census(),
                                      "schema": "probe-census/v9"}),
                          encoding="utf-8")
        stored = target.read_bytes()
        expect_refusal(
            lambda: census_storage.record_result(target, result_document()),
            "a census declaring an unreadable schema is refused",
            "probe-census/v9")
        expect(target.read_bytes() == stored, "...and changes no bytes")

        # -- the intake contract against the REAL producer -------------
        # `result_document()` is hand-written, so a schema that only
        # ever met it could agree with the fixture while both drifted
        # from what tools/probe_flake.py actually writes. This builds a
        # real `Measurement` — no engine, no subprocess beyond the `git`
        # call it makes for its own commit sha — and validates its own
        # serialization.
        descriptor = probe_flake.probe_protocol.build_descriptor(
            "alpha", [("first", "the first check"),
                      ("second", "the second check")])
        measurement = probe_flake.Measurement(
            "alpha", descriptor, requested_runs=2,
            rts_caps=probe_flake.DEFAULT_RTS_CAPS,
            artifact_root=Path("/tmp/artifacts"),
            invocation_dir=Path("/tmp/artifacts/alpha-1"))
        measurement.runs.append(probe_flake.RunRecord(
            1, 9100, probe_flake.RUN_PASS, 12.5,
            {"first": "PASS", "second": "PASS"}, None))
        measurement.runs.append(probe_flake.RunRecord(
            2, 9101, probe_flake.RUN_FAIL, 13.25,
            {"first": "PASS", "second": "FAIL"},
            Path("/tmp/artifacts/alpha-1/run-002")))
        expect_valid(
            lambda: census_contract.validate_result(measurement.to_document()),
            "the producer's own serialization satisfies the declared "
            "intake schema")
        expect(set(measurement.to_document()) == set(result_document()),
               "...and this file's fixture carries exactly its fields")
        # A real harness error on run 3 means three runs were REQUESTED
        # and the broken one never joined `runs`, so two completed —
        # which is #1493's cross-field rule as well as the producer's
        # own behaviour.
        measurement.requested_runs = 3
        measurement.status = "harness-error"
        measurement.error = "run 3 emitted a duplicate event"
        measurement.error_run = probe_flake.RunRecord(
            3, 9102, probe_flake.RUN_HARNESS_ERROR, 0.5, {},
            Path("/tmp/artifacts/alpha-1/run-003"))
        expect_valid(
            lambda: census_contract.validate_result(measurement.to_document()),
            "...and so does one carrying a real HARNESS_ERROR run")

        # -- and a valid document still goes all the way through -------
        expect_valid(lambda: census_storage.record_result(clean,
                                                        result_document()),
                     "a valid measurement is still accepted end to end")
        expect_valid(lambda: census_storage.record_result(clean,
                                                        harness_error_result()),
                     "...and so is a valid harness error")
        expect_valid(lambda: census_contract.validate_document(
            json.loads(clean.read_text(encoding="utf-8")),
            census_contract.CENSUS_SCHEMA, "the written census"),
            "and what the writer produced validates against its own schema")


# ==========================================================================
DRAFT = "https://json-schema.org/draft/2020-12/schema"


@contextmanager
def schema_file(text: str | None, root: Path):
    """`census_contract.SCHEMA_PATH` pointed at `text` (None = absent)."""
    target = root / f"schema-{abs(hash(text)) % 10 ** 8}.json"
    if text is not None:
        target.write_text(text, encoding="utf-8")
    saved = census_contract.SCHEMA_PATH
    cache = dict(census_contract._SCHEMA_CACHE)
    census_contract.SCHEMA_PATH = target
    census_contract._SCHEMA_CACHE.clear()
    try:
        yield target
    finally:
        census_contract.SCHEMA_PATH = saved
        census_contract._SCHEMA_CACHE.clear()
        census_contract._SCHEMA_CACHE.update(cache)


def _refuses_every_operation(census, before, good, fragment, why) -> None:
    """Every writing operation refuses `fragment`, and writes nothing."""
    for name, operation in (
        ("--record", lambda: census_storage.record_result(census, good)),
        ("--seed", lambda: census_storage.ensure_document(census)),
        ("a policy update", lambda: census_storage.record_policy(
            census, "alpha", acceptable_failures=1)),
    ):
        expect_refusal(operation, f"...and {name} refuses ({why})", fragment)
        expect(census.read_bytes() == before,
               f"...having written nothing ({name}, {why})")


def test_malformed_schema_file() -> None:
    """A broken SCHEMA is a refusal too, not a traceback.

    The validator's own input is a checked-in file, so it is exactly as
    capable of being wrong as the documents it validates — and a gate
    that dies with a stack trace on its own configuration is the failure
    mode this module exists to avoid. Every step of `load_schema` is
    ordered so the next one is safe to take; each case here is one of
    those steps, and every one also proves a real operation refused
    without writing.
    """
    print("\n-- a broken schema file refuses cleanly --")
    with registry(), scratch() as root:
        census = root / "probe_census.json"
        seeded(census)
        before = census.read_bytes()
        good = result_document()

        # Files `load_schema` itself must refuse, before it hands the
        # document to a library helper that would subscript it.
        unloadable = [
            # A valid-JSON schema that is not an object at all. The
            # library's `validator_for` SUBSCRIPTS what it is given, so
            # reaching it with a list raised out of the library.
            ('["$schema"]', "must be a JSON object", "a list"),
            ("5", "must be a JSON object", "a bare number"),
            ('"x"', "must be a JSON object", "a bare string"),
            ("true", "must be a JSON object", "a bare boolean"),
            ("null", "must be a JSON object", "a bare null"),
            ("{oops", "is not valid JSON", "text that is not JSON"),
            ("{}", "does not identify a JSON Schema draft", "no `$schema`"),
            ('{"$schema": 5}', "does not identify a JSON Schema draft",
             "a numeric `$schema`"),
            ('{"$schema": [1]}', "does not identify a JSON Schema draft",
             "an unhashable `$schema`"),
            ('{"$schema": "https://example.invalid/draft/9"}',
             "not a draft this jsonschema implements", "an unknown draft"),
            (json.dumps({"$schema": DRAFT,
                         "$defs": {"census_v2": {"type": 5}}}),
             "is not a valid", "a draft-invalid schema body"),
            (None, "is unreadable", "no schema file at all"),
        ]
        # And files that LOAD but cannot be applied: a schema can be a
        # perfectly valid schema and still not describe this tool's
        # documents, or not resolve its own references.
        # Every root definition dangles, so the failure is reached
        # whichever document an operation validates first.
        dangling = {"$schema": DRAFT,
                    "$defs": {name: {"$ref": "#/$defs/gone"}
                              for name in census_contract.SCHEMA_DEFINITIONS
                              .values()}}
        unusable = [
            (json.dumps({"$schema": DRAFT, "$defs": {"nothing": True}}),
             "declares no", "no definition for the documents it validates"),
            (json.dumps(dangling), "could not be applied",
             "a `$ref` naming nothing"),
        ]

        for text, fragment, why in unloadable:
            with schema_file(text, root):
                expect_refusal(census_contract.load_schema,
                               f"loading the schema refuses {why}", fragment)
                _refuses_every_operation(census, before, good, fragment, why)
        for text, fragment, why in unusable:
            with schema_file(text, root):
                expect_valid(census_contract.load_schema,
                             f"the schema itself loads with {why}")
                _refuses_every_operation(census, before, good, fragment, why)

        # And the shipped schema still loads, so no case above leaked
        # global state into the rest of the suite.
        expect_valid(census_contract.load_schema,
                     "the shipped schema still loads afterwards")
        expect_valid(lambda: census_storage.record_result(census, good),
                     "...and a real measurement still records")


# ==========================================================================
class _BlockedImport:
    """A meta-path finder that makes one package deterministically absent."""

    def __init__(self, name: str):
        self.name = name

    def find_spec(self, fullname, path=None, target=None):
        if fullname == self.name or fullname.startswith(f"{self.name}."):
            raise ImportError(
                f"blocked by the missing-dependency case: {fullname}")
        return None


@contextmanager
def without_jsonschema():
    """`import jsonschema` fails for the duration, and nothing else does.

    Deliberately NOT a monkeypatched flag inside `probe_census`: the
    promise under test is about the ENVIRONMENT, so the import itself
    has to fail. Any already-imported submodule is purged too, or the
    blocked import would be served from `sys.modules`.
    """
    blocked = _BlockedImport("jsonschema")
    purged = {name: module for name, module in sys.modules.items()
              if name == "jsonschema" or name.startswith("jsonschema.")}
    for name in purged:
        del sys.modules[name]
    sys.meta_path.insert(0, blocked)
    try:
        yield
    finally:
        sys.meta_path.remove(blocked)
        sys.modules.update(purged)


def test_missing_dependency() -> None:
    """An absent `jsonschema` is one loud error, never a silent skip.

    A validator that quietly enforces nothing is worse than no validator
    at all: the run looks clean and the gate is gone. So every case here
    asserts BOTH halves — the refusal happened, and the operation it
    refused wrote nothing.
    """
    print("\n-- an absent jsonschema refuses loudly --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        before = path.read_bytes()
        good = result_document()
        # Warm every cache first. `_require_jsonschema` runs BEFORE the
        # schema and validator caches are consulted precisely so a
        # previously working environment cannot satisfy a later
        # validation, and priming here is what proves it.
        census_contract.validate_result(good)
        census_contract.load_schema()

        with without_jsonschema():
            expect_refusal(lambda: census_contract.validate_result(good),
                           "validation refuses when the library is absent",
                           "jsonschema is required", census_contract.INSTALL_HINT)
            expect_refusal(lambda: census_contract.load_schema(),
                           "a primed schema cache does not satisfy it either",
                           census_contract.INSTALL_HINT)
            for name, operation in (
                ("--record", lambda: census_storage.record_result(path, good)),
                ("--seed", lambda: census_storage.ensure_document(path)),
                ("a policy update", lambda: census_storage.record_policy(
                    path, "alpha", acceptable_failures=1)),
            ):
                expect_refusal(operation,
                               f"{name} refuses without the library",
                               census_contract.INSTALL_HINT)
                expect(path.read_bytes() == before,
                       f"...and {name} wrote nothing")

        expect_valid(lambda: census_contract.validate_result(good),
                     "and validation works again once the library is back")

    # Through the CLI, where the exit code and the streams are the
    # contract: one non-zero refusal, the install command named once,
    # no traceback, and no success line.
    with registry(ci_eligible={"beta"}), cli_repo() as (_root, path):
        cli("--seed")
        before = path.read_bytes()
        holding = Path(tempfile.mkdtemp(prefix="probe-census-nodep-"))
        try:
            result_file = holding / "result.json"
            result_file.write_text(json.dumps(result_document()),
                                   encoding="utf-8")
            with without_jsonschema():
                code, out, err = cli("--record", str(result_file))
                expect(code == 1,
                       "--record exits non-zero with no jsonschema")
                expect(err.count(census_contract.INSTALL_HINT) == 1,
                       "...naming the install command exactly once")
                expect("Traceback" not in err, "...with no traceback")
                expect(out == "", "...and printing no success line")
                expect(path.read_bytes() == before,
                       "...and leaving the census byte-for-byte alone")
                code, _, err = cli("--validate")
                expect(code == 1 and census_contract.INSTALL_HINT in err,
                       "--validate refuses rather than validating nothing")
                expect(path.read_bytes() == before, "...and changes no bytes")
                # `--print` reads and writes nothing and validates
                # nothing — it renders the live registry — so it is not
                # a skipped check, and keeping it dependency-free is
                # what lets a fresh checkout run it.
                code, out, _ = cli("--print")
                expect(code == 0 and '"probe-census/v5"' in out,
                       "--print still works: it validates nothing to skip")
            code, _, _ = cli("--record", str(result_file))
            expect(code == 0,
                   "and the same command succeeds once the library is back")
        finally:
            shutil.rmtree(holding, ignore_errors=True)


# ==========================================================================
# The cross-field invariants (#1493)
# ==========================================================================

def _harness_attempt(accepted: bool, **overrides) -> dict:
    """One well-formed harness-error attempt, from the real summarizer."""
    record = census_records.summarize_attempt(
        result_document(status="harness-error"), accepted)
    record.update(overrides)
    return record


def _drop_attempt(document: dict) -> None:
    _alpha(document)["attempts"].pop()


def _lose_the_samples(document: dict) -> None:
    census = _alpha(document)
    census["current"] = None
    census["history"] = []


def _forge_accepted_flag(document: dict) -> None:
    _alpha(document)["attempts"][0]["accepted"] = False


def _forge_harness_accepted(document: dict) -> None:
    _alpha(document)["attempts"].append(_harness_attempt(True))


def _finished_harness_error(document: dict) -> None:
    _alpha(document)["attempts"].append(
        _harness_attempt(False, requested_runs=2, completed_runs=2))


def _misfiled_sample(document: dict) -> None:
    _alpha(document)["current"]["samples"][0]["commit_sha"] = COMMIT_B


def _blank_deferral_resume_condition(document: dict) -> None:
    _alpha(document)["deferred"] = {
        "reason": "tree assets are incomplete",
        "resume_when": "   ",
    }


# Each stored case breaks exactly ONE relationship while staying
# schema-valid, and names the rule that must be the one rejecting it.
CENSUS_CASES = (
    (census_contract._rule_attempts_reconcile_with_samples,
     "accepted attempts left behind by cleared cohorts",
     "logs 2 accepted attempt(s) but retains 0 sample(s)",
     _lose_the_samples),
    (census_contract._rule_attempts_reconcile_with_samples,
     "a sample with no accepted attempt to log it",
     "logs 1 accepted attempt(s) but retains 2 sample(s)",
     _drop_attempt),
    (census_contract._rule_accepted_derives_from_status,
     "`accepted` false beside an accepted status",
     "`accepted` is derived from `status`",
     _forge_accepted_flag),
    (census_contract._rule_accepted_derives_from_status,
     "`accepted` true beside a harness error",
     "`accepted` is derived from `status`",
     _forge_harness_accepted),
    (census_contract._rule_attempt_leaves_a_run_uncompleted,
     "a logged harness error that completed every run",
     "reports completing 2 of 2 requested run(s)",
     _finished_harness_error),
    (census_contract._rule_cohort_holds_one_commit,
     "a sample filed under another commit's cohort",
     "a cohort holds one commit's samples",
     _misfiled_sample),
    (census_contract._rule_deferral_is_actionable,
     "a deferral with a blank resume condition",
     "has no non-blank deferral resume when",
     _blank_deferral_resume_condition),
)


def _pass_run_fails_a_check(result: dict) -> None:
    result["runs"][0]["checks"]["second"] = "FAIL"
    # Kept in step so the tally rule has nothing to say about it.
    result["check_counts"]["second"] = {"PASS": 0, "FAIL": 2, "MISSING": 0}


def _wrong_tally(result: dict) -> None:
    result["check_counts"]["first"]["PASS"] = 1


def _untallied_check(result: dict) -> None:
    """A check a run reports that `check_counts` has no entry for.

    Undeclared on BOTH sides, so the descriptor-coverage rule sees a map
    still keyed by exactly the declared checks and stays silent: the
    tally rule is the only one that can notice the run.
    """
    result["runs"][0]["checks"]["ghost"] = "PASS"


def _undeclared_tally(result: dict) -> None:
    """An entry for a check the descriptor never declared.

    Its tally is all zero, which is precisely what the per-entry
    comparison accepts — no run shows anything for it, so the numbers
    agree. Only the keying rule can reject it.
    """
    result["check_counts"]["ghost"] = {"PASS": 0, "FAIL": 0, "MISSING": 0}


def _untallied_declared_check(result: dict) -> None:
    """A declared check with no entry, in a measurement with NO runs.

    A harness error on the very first run completes nothing, so every
    entry is all zero and the runs show nothing at all — which is what
    leaves the tally rule with nothing to say and isolates the loss of
    the key itself.
    """
    result.update(_no_runs_result())
    del result["check_counts"]["second"]


def _finished_harness_result(result: dict) -> None:
    """A real harness error, then its broken run counted as completed.

    Built from the realistic fixture rather than by flipping an accepted
    measurement's status, so `completed_runs` is the only field left
    disagreeing with the rest.
    """
    result.update(result_document(status="harness-error"))
    result["completed_runs"] = result["requested_runs"]


RESULT_CASES = (
    (census_contract._rule_pass_run_has_no_failed_check,
     "a PASS run carrying a FAIL check",
     "a failed check makes its run fail",
     _pass_run_fails_a_check),
    (census_contract._rule_check_counts_tally_runs,
     "a tally that is not what the runs show",
     "is not the PASS=2 FAIL=0 MISSING=0 `runs` shows",
     _wrong_tally),
    (census_contract._rule_check_counts_tally_runs,
     "a check tallied in the runs with no entry",
     "but has no entry",
     _untallied_check),
    (census_contract._rule_check_counts_cover_the_descriptor,
     "an all-zero tally for a check the descriptor never declared",
     "the probe's own descriptor does not declare it",
     _undeclared_tally),
    (census_contract._rule_check_counts_cover_the_descriptor,
     "a declared check with no tally at all",
     "declared check 'second' has no tally",
     _untallied_declared_check),
    (census_contract._rule_result_leaves_a_run_uncompleted,
     "a harness error that completed every requested run",
     "reports completing 2 of 2 requested run(s)",
     _finished_harness_result),
)


@contextmanager
def without_rule(rule):
    """The production rule set with exactly `rule` lifted out of it.

    This is the mutation check the issue requires, run rather than
    asserted: with one rule gone its own case must be ACCEPTED, which is
    what proves that rule is the one rejecting it and that the fixture
    isolates a single relationship. A neighbouring rule catching the
    same fixture would keep refusing it here and fail the case.
    """
    saved = (census_contract.CENSUS_RULES, census_contract.RESULT_RULES)
    census_contract.CENSUS_RULES = tuple(
        r for r in census_contract.CENSUS_RULES if r is not rule)
    census_contract.RESULT_RULES = tuple(
        r for r in census_contract.RESULT_RULES if r is not rule)
    try:
        yield
    finally:
        census_contract.CENSUS_RULES, census_contract.RESULT_RULES = saved


def test_cross_field_invariants() -> None:
    """The rules that span fields, which no schema keyword can state.

    Three things are proved for every rule, because "reject malformed
    input" fails by over-rejecting just as readily as by
    under-rejecting: a rejecting case driven through a REAL operation on
    a real census (refused, and not one byte rewritten), a positive case
    built the way the producer really builds one, and a mutation check
    that lifts that one rule out of the production rule set and requires
    its own case to be accepted again.
    """
    print("\n-- the cross-field invariants --")
    expect(len(census_contract.CENSUS_RULES) == len(
        {rule for rule, _why, _fragment, _mutate in CENSUS_CASES}),
        "every stored rule has a case of its own")
    expect(len(census_contract.RESULT_RULES) == len(
        {rule for rule, _why, _fragment, _mutate in RESULT_CASES}),
        "every intake rule has a case of its own")

    with registry(), scratch() as root:
        target = root / "probe_census.json"
        clean = root / "clean.json"
        census_storage.ensure_document(clean)
        clean_bytes = clean.read_bytes()

        # -- stored state: refused by every operation, and unchanged ---
        # A census this tool cannot trust must stop `--record`, the
        # policy updates, `--seed` and `--validate` alike: the defect
        # #1493 was filed for is that they each rewrote the file and so
        # made the inconsistency durable.
        operations = (
            ("--record", lambda p: census_storage.record_result(
                p, result_document(commit=COMMIT_B))),
            ("--set-acceptable-failures", lambda p: census_storage.record_policy(
                p, "alpha", acceptable_failures=1)),
            ("--seed", census_storage.ensure_document),
            ("--validate", lambda p: census_contract.validate_census(
                census_storage.load(p), f"census {p}")),
        )
        for rule, why, fragment, mutate in CENSUS_CASES:
            document = rich_census()
            mutate(document)
            target.write_text(json.dumps(document), encoding="utf-8")
            stored = target.read_bytes()
            expect_valid(
                lambda d=document: census_contract.validate_document(
                    d, census_contract.CENSUS_SCHEMA, "the case"),
                f"a census with {why} is still SCHEMA-valid, so only the "
                f"cross-field rule can reject it")
            for name, operation in operations:
                expect_refusal(lambda o=operation: o(target),
                               f"{name} refuses a census with {why}", fragment)
                expect(target.read_bytes() == stored,
                       f"...and rewrote nothing ({name}, {why})")
            with without_rule(rule):
                expect_valid(
                    lambda d=document: census_contract.validate_census(
                        d, "the case"),
                    f"mutation check: without {rule.__name__}, {why} is "
                    f"accepted — that rule is what rejects it")

        # -- the intake surface: refused, and nothing written ----------
        for rule, why, fragment, mutate in RESULT_CASES:
            document = result_document()
            mutate(document)
            expect_valid(
                lambda d=document: census_contract.validate_document(
                    d, census_contract.RESULT_SCHEMA, "the case"),
                f"a result with {why} is still SCHEMA-valid")
            expect_refusal(
                lambda d=document: census_storage.record_result(clean, d),
                f"--record refuses a result with {why}", fragment)
            expect(clean.read_bytes() == clean_bytes,
                   f"...and wrote nothing ({why})")
            with without_rule(rule):
                expect_valid(
                    lambda d=document: census_contract.validate_result(d),
                    f"mutation check: without {rule.__name__}, {why} is "
                    f"accepted — that rule is what rejects it")

        # The harness-error relationship is guarded on BOTH surfaces,
        # deliberately: the intake rule refuses the document, and the
        # stored rule refuses the attempt it would have become. Lifting
        # the intake one out therefore does NOT let the write through —
        # which is the defence in depth, and worth pinning rather than
        # hiding inside a mutation check that dodges it.
        finished = result_document()
        _finished_harness_result(finished)
        with without_rule(census_contract._rule_result_leaves_a_run_uncompleted):
            expect_refusal(
                lambda: census_storage.record_result(clean, finished),
                "the stored rule still catches a finished harness error "
                "the intake rule was not there to refuse",
                "reports completing 2 of 2 requested run(s)")
        expect(clean.read_bytes() == clean_bytes,
               "...and that refusal wrote nothing either")

    # -- the positive half: every legitimate flow still goes through ---
    # Over-rejection is this kind of rule's real failure mode, so the
    # accepting cases are the load-bearing ones. They come from the
    # producer's own serialization where they can, not from a
    # hand-written document that could agree with the fixtures while
    # both drifted from what probe_flake.py writes.
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        expect(census_contract.census_invariants(census_records.build_manifest())
               == [], "an empty census reconciles 0 accepted against 0 "
                      "retained")
        census_storage.ensure_document(path)

        descriptor = probe_flake.probe_protocol.build_descriptor(
            "alpha", [("first", "the first check"),
                      ("second", "the second check")])

        def measurement(requested: int) -> probe_flake.Measurement:
            return probe_flake.Measurement(
                "alpha", descriptor, requested_runs=requested,
                rts_caps=probe_flake.DEFAULT_RTS_CAPS,
                artifact_root=Path("/tmp/artifacts"),
                invocation_dir=Path("/tmp/artifacts/alpha-1"))

        # A TIMEOUT wins outright, so a timed-out run really can carry a
        # FAIL check. The PASS rule must not reach it.
        timed_out = measurement(1)
        timed_out.runs.append(probe_flake.RunRecord(
            1, 9100, probe_flake.RUN_TIMEOUT, 900.0,
            {"first": "PASS", "second": "FAIL"},
            Path("/tmp/artifacts/alpha-1/run-001")))
        expect_valid(
            lambda: census_storage.record_result(path, timed_out.to_document()),
            "a TIMEOUT run carrying a FAIL check is accepted")

        # A harness error on the very FIRST run completes nothing, so
        # `check_counts` is the descriptor seeded to all zeros — the one
        # legitimate shape in which an entry counts nothing, and the
        # reason the keying rule cannot be inferred from the tallies.
        nothing_ran = measurement(3)
        nothing_ran.status = "harness-error"
        nothing_ran.error = "run 1 emitted a duplicate event"
        nothing_ran.error_run = probe_flake.RunRecord(
            1, 9101, probe_flake.RUN_HARNESS_ERROR, 0.5, {},
            Path("/tmp/artifacts/alpha-1/run-001"))
        produced = nothing_ran.to_document()
        expect_valid(
            lambda: census_storage.record_result(path, produced),
            "a harness error that completed no run at all is accepted")
        expect(set(produced["check_counts"])
               == {check["id"] for check in produced["checks"]}
               and all(sum(tally.values()) == 0
                       for tally in produced["check_counts"].values()),
               "the producer keys check_counts by exactly its descriptor, "
               "all zero when no run completed")
        expect(set(_no_runs_result()["check_counts"])
               == set(produced["check_counts"]),
               "...and this file's no-runs fixture is keyed the same way")
        expect_valid(lambda: census_contract.validate_result(_no_runs_result()),
                     "...so the fixture the keying case builds on is itself "
                     "consistent")

        # And the same keying holds once runs DO complete.
        expect(set(result_document()["check_counts"])
               == {check["id"] for check in result_document()["checks"]},
               "a completed measurement is keyed by its descriptor too")

        # Two measurements of ONE commit accumulate in a single cohort;
        # a third naming another commit rolls that whole cohort into
        # history. Both are legitimate retention, and the equality has
        # to survive each. (The producer-built measurements above stamp
        # the REAL checkout commit, so they sit in a cohort of their
        # own — which is exactly the multi-cohort state the equality is
        # summed across.)
        for index in (1, 2):
            expect_valid(lambda: census_storage.record_result(path,
                                                            result_document()),
                         f"accepted measurement {index} of one commit is "
                         f"accepted")
        rollover = result_document(commit=COMMIT_B)
        expect_valid(
            lambda: census_storage.record_result(path, rollover),
            "and one naming a new commit, which rolls the cohort over")
        stored = json.loads(path.read_text(encoding="utf-8"))
        census = stored["probes"][0]["census"]
        archived = {cohort["commit_sha"]: len(cohort["samples"])
                    for cohort in census["history"]}
        expect(archived.get(COMMIT_A) == 2
               and census["current"]["commit_sha"] == COMMIT_B
               and len(census["current"]["samples"]) == 1,
               f"the rollover archived the two-sample cohort rather than "
               f"dropping it ({archived})")
        expect(len(census["attempts"]) == 5
               and sum(1 for a in census["attempts"]
                       if a["status"] == "ok") == 4,
               "and the harness error is logged without a sample")
        expect(census_contract.census_invariants(stored) == [],
               "accepted attempts still reconcile against retained samples "
               "across current AND history")

        # Promotion archives the current cohort; reconciliation appends
        # rows. Neither may disturb the equality.
        cohorts = len(census["history"])
        with registry(ci_eligible={"alpha"}):
            promoted = census_storage.ensure_document(path)
        expect(promoted["probes"][0]["census"]["current"] is None
               and len(promoted["probes"][0]["census"]["history"])
               == cohorts + 1,
               "promotion archived the current cohort rather than dropping it")
        expect_valid(
            lambda: census_contract.validate_census(promoted, "the promoted "
                                                           "census"),
            "a promoted, reconciled census still satisfies every invariant")


#: This family's complete ordered inventory, and the whole of its
#: contribution to the aggregate: the two rule families run before the
#: three schema-file ones, which is the order this gate has always had.
TESTS = (
    test_adversarial_malformed_input,
    test_cross_field_invariants,
    test_declared_schema,
    test_malformed_schema_file,
    test_missing_dependency,
)
