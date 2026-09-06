#!/usr/bin/env python3
"""#1437's cases: the entry gate, the routes, and the diagnosis CLI.

The 185 cases covering `tools/deflake_diagnosis.py` — the handoff entry
gate, X out of ten, the no-retries rule, the configuration manifest,
controlled reproduction, same-environment verification, stable check
identity, MISSING, every route, assertion weakening, the frozen repair,
the one-PR limit, the command line, the constants that must not drift,
and the mutation evidence behind all of it.

`mutant` and `check_mutation` live HERE rather than in the shared
support module. Every one of their call sites is a diagnosis case, and
the harness they implement is diagnosis-shaped: it reads
`deflake_diagnosis.py`'s own source, applies one textual substitution
and `exec`s the result into a throwaway `types.ModuleType`. The
IMPORTED production module is never mutated, so there is nothing to
restore and no teardown to share — the private compiled copy is the
isolation.

Not a gate of its own. Run through the facade:

  python3 tools/test_deflake_diagnosis.py --only diagnosis
"""
from __future__ import annotations

import copy
import json
import re
import shutil
import subprocess
import sys
import tempfile
import types
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import deflake  # type: ignore  # noqa: E402
import deflake_diagnosis as dd  # type: ignore  # noqa: E402
import probe_census  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
import probe_runner_registry  # type: ignore  # noqa: E402
from deflake_diagnosis_selftest_support import (  # noqa: E402
    BASE_COMMIT, CHECKS, CLEAN_WT, FAIL, FAILURES, MISSING, OTHER, OUTSIDE,
    PASS, PRIMARY_WT, PROBE, PRODUCER_FIELD, REPAIR_COMMIT, REPAIR_WT, TOOL,
    VERIFY_ARTIFACTS, WORKTREES, batch_section, command, config_entries,
    deflake_argv, deflake_invocation, diagnosis_document, evaluate, expect,
    expect_refused, expect_rejected, failing_runs, handoff_document,
    invocation, manifest, relocate_section, resource_hold, result_document,
    verification_result)

#: A directory with no `config/*.local.yaml` family at all, so
#: `deflake.configuration_manifest` answers with the empty list that is
#: this repository's expected default.
#:
#: Created on FIRST USE, not at import. It is diagnosis-owned with a
#: single consumer, and `--only outcome` / `--only issue` still import
#: this module so the facade can read its registry; a temporary
#: directory those invocations never look at is footprint the split
#: must not add. Like the module-level `mkdtemp` it replaces, it is
#: created once per process and deliberately never removed.
_PRIMARY_CONFIG_ROOT: str | None = None


def primary_config_root() -> str:
    """The empty configuration root, created the first time it is read."""
    global _PRIMARY_CONFIG_ROOT
    if _PRIMARY_CONFIG_ROOT is None:
        _PRIMARY_CONFIG_ROOT = tempfile.mkdtemp(prefix="deflake_diag_config_")
    return _PRIMARY_CONFIG_ROOT


# ==========================================================================
# The entry gate: complete and malformed handoffs
# ==========================================================================
def test_a_complete_handoff_is_accepted() -> None:
    handoff = dd.require_handoff(handoff_document())
    expect(handoff.probe == PROBE, "the accepted handoff names its probe")
    expect(handoff.acceptable_failures == 0, "X survives the gate")
    expect(handoff.expected_checks == [cid for cid, _l in CHECKS],
           "the ordered descriptor comes from the result document itself")
    expect(handoff.commit_sha == BASE_COMMIT,
           "the baseline commit is the result document's own")


def test_a_handoff_naming_several_probes_is_refused() -> None:
    document = handoff_document()
    document["probe"] = [PROBE, OTHER]
    expect_rejected(lambda: dd.require_handoff(document), "names 2 probes",
                    "a handoff naming two probes")


def test_an_unregistered_probe_is_refused() -> None:
    document = handoff_document()
    document["probe"] = "not_a_real_probe"
    document["result"]["probe"] = "not_a_real_probe"
    expect_rejected(lambda: dd.require_handoff(document),
                    "not a registered probe key",
                    "a handoff naming an unregistered probe")


def test_a_probe_with_no_descriptor_is_refused() -> None:
    """A legacy probe has no per-check evidence to diagnose."""
    legacy = next(key for key, _script, _purpose in probe_runner_registry.PROBES
                  if key not in probe_flake.PROTOCOL_PROBES)
    document = handoff_document(probe=legacy)
    document["result"]["probe"] = legacy
    expect_rejected(lambda: dd.require_handoff(document),
                    "does not implement", "a legacy probe handoff")


def test_the_wrong_schema_is_refused() -> None:
    document = handoff_document()
    document["schema"] = "deflake-handoff/v0"
    expect_rejected(lambda: dd.require_handoff(document), "expected",
                    "a handoff with the wrong schema")


def test_a_handoff_rebuilt_from_the_census_row_is_refused() -> None:
    """The durable census row is not a substitute for the result document.

    `probe_census.ingest_result` deliberately drops the ports, the
    per-run check maps, the descriptor labels, the artifact root, the
    invocation directory and the exact command — so a handoff carrying
    only what survived ingestion cannot identify the baseline
    invocation, and this is the shape that arrives when someone tries.
    """
    document = handoff_document()
    document["result"] = None
    expect_rejected(lambda: dd.require_handoff(document),
                    "durable census row is not a substitute",
                    "a handoff with no embedded result document")


def test_a_handoff_whose_result_measured_another_probe_is_refused() -> None:
    document = handoff_document()
    document["result"] = result_document(probe=OTHER, runs=failing_runs(3))
    expect_rejected(lambda: dd.require_handoff(document),
                    "its result document measured",
                    "a handoff whose result is another probe's")


def test_the_targets_are_every_non_pass_identifier_in_order() -> None:
    """Not a selection FROM the measurement — they are it."""
    accepted = dd.require_handoff(handoff_document())
    expect(list(accepted.targets) == ["beta", "gamma"],
           f"an abort at beta implicates gamma too; got {accepted.targets}")

    expect_rejected(lambda: dd.require_handoff(handoff_document(
        targets=("delta",))),
        "identifiers the descriptor never declared",
        "a target that is not a declared check")
    expect_rejected(lambda: dd.require_handoff(handoff_document(
        targets=("beta",))),
        "naming a subset would let a repair be verified",
        "a target list that omits an observed failure")
    expect_rejected(lambda: dd.require_handoff(handoff_document(
        targets=("alpha", "beta", "gamma"))),
        "targets something this measurement did not see",
        "a target that never went non-PASS")
    expect_rejected(lambda: dd.require_handoff(handoff_document(
        targets=("gamma", "beta"))),
        "in that order",
        "targets in an order the descriptor does not declare")
    expect_rejected(lambda: dd.require_handoff(handoff_document(
        targets=("beta", "beta", "gamma"))),
        "repeats an identifier",
        "a repeated target")


def test_an_emptied_target_list_still_has_to_match_the_measurement() -> None:
    """Emptying the list under a FAILING measurement is still a lie.

    An empty `targets` is only legitimate when the measurement itself
    observed nothing non-PASS; here it contradicts two observed failures,
    and the equality rule says so.
    """
    document = handoff_document(targets=())
    expect_rejected(lambda: dd.require_handoff(document),
                    "All of them are diagnosis inputs",
                    "an emptied target list over a failing measurement")

    document = handoff_document()
    del document["targets"]
    expect_rejected(lambda: dd.require_handoff(document),
                    "absence has to be asserted",
                    "a handoff with no `targets` key at all")


def test_an_all_pass_handoff_is_the_no_target_outcome() -> None:
    """`/deflake` writes one, so the gate may not call it malformed.

    `deflake.handoff_targets` returns `[]` for an all-PASS measurement
    and `tools/test_deflake.py` pins that case, so this is a legitimate
    input with nothing to diagnose. The approved correction routes it to
    #1439 instead of rejecting it or inventing a target.
    """
    passing = handoff_document(result=result_document())
    expect(passing["targets"] == [],
           f"the producer derives no target from an all-PASS measurement; "
           f"got {passing['targets']}")
    accepted = dd.require_handoff(copy.deepcopy(passing))
    expect(accepted.targets == (),
           "and the entry gate admits it rather than refusing it")

    document = diagnosis_document(route=dd.ROUTE_NO_TARGET, handoff=passing,
                                  baseline=False, verification=False)
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_NO_TARGET,
           f"an all-PASS handoff is the no-target outcome; got {outcome.route}")
    expect(outcome.owner_issue == 1439,
           f"owned by #1439; got {outcome.owner_issue}")
    expect(not outcome.opens_pull_request,
           "and it opens no pull request")
    expect(outcome.targets == [], "with no targets to report")

    # Any other route over the same handoff is mislabelled.
    for route in (dd.ROUTE_REPAIR, dd.ROUTE_CANNOT_REPRODUCE,
                  dd.ROUTE_PRODUCTION_DEFECT):
        mislabelled = diagnosis_document(route=route, handoff=passing,
                                         baseline=False, verification=False)
        expect_refused(lambda d=mislabelled: evaluate(d),
                       "no target to diagnose",
                       f"an all-PASS handoff declared as {route!r}")

    # And the no-target route over a handoff that DOES name targets.
    inverted = diagnosis_document(route=dd.ROUTE_NO_TARGET)
    expect_refused(lambda: evaluate(inverted),
                   "what an all-PASS measurement produces",
                   "the no-target route over a handoff naming targets")


def test_the_no_target_route_runs_no_controlled_batch() -> None:
    """It stops before creating repair work, so a batch is work it forbids."""
    passing = handoff_document(result=result_document())
    for section in ("baseline", "verification"):
        document = diagnosis_document(route=dd.ROUTE_NO_TARGET,
                                      handoff=passing, baseline=False,
                                      verification=False)
        document[section] = batch_section()
        expect_refused(lambda d=document: evaluate(d),
                       f"runs no {section} batch",
                       f"a no-target diagnosis carrying a {section}")


def test_an_expected_check_list_that_contradicts_the_descriptor_is_refused() -> None:
    document = handoff_document()
    document["expected_checks"] = ["alpha", "gamma", "beta"]
    expect_rejected(lambda: dd.require_handoff(document),
                    "the descriptor is the ordered contract",
                    "a reordered expected-check list")


def test_the_retained_list_is_exactly_what_the_runs_kept() -> None:
    """So "failures with no evidence" is unrepresentable, not just refused."""
    document = handoff_document()
    document["result"]["retained_artifacts"] = []
    expect_rejected(lambda: dd.require_handoff(document),
                    "naming evidence it does not have",
                    "a document that dropped its retained list")

    document = handoff_document()
    document["result"]["retained_artifacts"].append(f"{OUTSIDE}/invented")
    expect_rejected(lambda: dd.require_handoff(document),
                    "naming evidence it does not have",
                    "a document that invented a retained path")


def test_a_passing_run_keeps_no_raw_artifacts() -> None:
    """One of verification's own success conditions, and a harness fact.

    `probe_flake.measure` deletes a run's directory the moment it passes
    and records `artifact_dir: null`, so a PASS run naming one was not
    written by the harness — and leaving successful-run artifacts behind
    is exactly what a verification batch may not do.
    """
    document = handoff_document()
    passing = next(run for run in document["result"]["runs"]
                   if run["outcome"] == probe_flake.RUN_PASS)
    passing["artifact_dir"] = f"{OUTSIDE}/artifacts/invocation/kept"
    document["result"]["retained_artifacts"].append(passing["artifact_dir"])
    expect_rejected(lambda: dd.require_handoff(document),
                    "passed and still names the artifact directory",
                    "a passing run that kept its directory")

    end_to_end = diagnosis_document()
    kept = end_to_end["verification"]["result"]["runs"][0]
    kept["artifact_dir"] = f"{VERIFY_ARTIFACTS}/invocation/kept"
    end_to_end["verification"]["result"]["retained_artifacts"] = [
        kept["artifact_dir"]]
    expect_rejected(lambda: evaluate(end_to_end),
                    "passed and still names the artifact directory",
                    "a verification batch that kept a passing run")


def test_an_unsuccessful_run_must_still_have_its_logs() -> None:
    document = handoff_document()
    failing = next(run for run in document["result"]["runs"]
                   if run["outcome"] != probe_flake.RUN_PASS)
    document["result"]["retained_artifacts"].remove(failing["artifact_dir"])
    failing["artifact_dir"] = None
    expect_rejected(lambda: dd.require_handoff(document),
                    "a failure whose logs are gone",
                    "a failing run whose artifacts were discarded")


# ==========================================================================
# X: the numeric ceiling out of ten
# ==========================================================================
def test_x_must_be_a_validated_integer() -> None:
    for value in (None, "1", 1.0, True, -1, dd.RUN_COUNT):
        document = handoff_document(acceptable=0)
        document["acceptable_failures"] = value
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "acceptable_failures",
                        f"an X of {value!r}")


def test_x_is_the_census_policys_own_arithmetic() -> None:
    """At or below X passes, above it does not — X=1 accepts 1, rejects 2.

    The accepted failing run is NON-aborting (`abort=False`): a target
    has zero MISSING across all ten runs, so a run that aborted before
    one would be refused for that instead, and this case is about the
    arithmetic.
    """
    handoff = handoff_document(acceptable=1)
    accepted = diagnosis_document(handoff=handoff)
    accepted["verification"]["result"] = verification_result(
        runs=failing_runs(1, abort=False))
    outcome = evaluate(accepted)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"one failure against X=1 is a repair, got {outcome.route}")
    expect(outcome.verification_failures == 1, "the count is reported")

    over = diagnosis_document(handoff=handoff_document(acceptable=1))
    over["verification"]["result"] = verification_result(
        runs=failing_runs(2, abort=False))
    expect_refused(lambda: evaluate(over), "partial-improvement",
                   "two failures against X=1")


def test_x_zero_requires_a_spotless_batch() -> None:
    document = diagnosis_document()
    document["verification"]["result"] = verification_result(runs=failing_runs(1))
    expect_refused(lambda: evaluate(document), "partial-improvement",
                   "one failure against X=0")


def test_a_batch_of_the_wrong_size_is_not_a_measurement() -> None:
    short = result_document(runs=failing_runs(3)[:5], requested=5)
    document = handoff_document(result=short)
    expect_rejected(lambda: dd.require_handoff(document),
                    "measurement contract is exactly",
                    "a five-run batch")


def test_an_incomplete_batch_is_not_a_measurement() -> None:
    document = handoff_document()
    document["result"]["completed_runs"] = dd.RUN_COUNT - 1
    expect_rejected(lambda: dd.require_handoff(document),
                    "an incomplete batch is not a measurement",
                    "an incomplete batch")


def test_a_harness_error_is_not_a_comparison_side() -> None:
    document = handoff_document(result=result_document(
        runs=failing_runs(3), harness_error=True))
    expect_rejected(lambda: dd.require_handoff(document),
                    "no trustworthy failure rate",
                    "a harness-error batch")


def test_the_capability_count_is_fixed() -> None:
    document = handoff_document(result=result_document(
        runs=failing_runs(3), rts_caps=2))
    expect_rejected(lambda: dd.require_handoff(document),
                    "RTS capabilities", "a batch at two capabilities")


def test_an_overlapping_harness_invalidates_the_control() -> None:
    document = handoff_document()
    document["result"]["peak_concurrency"] = 2
    expect_rejected(lambda: dd.require_handoff(document),
                    "peak concurrency", "a contended batch")


def test_a_pass_run_carrying_a_failed_check_is_refused() -> None:
    """Delegation to `probe_census.validate_result`, and why it matters.

    `failure_count` counts RUNS by their outcome, so a document whose
    runs all claim PASS while their check maps carry FAIL would read as a
    spotless batch and could be admitted as a verified repair. The
    canonical validator's `_rule_pass_run_has_no_failed_check` is what
    refuses it, which is why every result document goes through the
    shipped validator before a single field is read.
    """
    document = handoff_document()
    result = document["result"]
    for run in result["runs"]:
        run["outcome"] = probe_flake.RUN_PASS
        run["checks"]["beta"] = FAIL
        run["checks"]["gamma"] = PASS
    result["check_counts"]["beta"] = {PASS: 0, FAIL: dd.RUN_COUNT, MISSING: 0}
    result["check_counts"]["gamma"] = {PASS: dd.RUN_COUNT, FAIL: 0,
                                       MISSING: 0}
    result["failure_count"] = 0
    result["failure_rate"] = 0.0
    result["timeout_count"] = 0
    expect(dd.failure_count(result) == 0,
           "the run-outcome count really would read this as spotless")
    expect_rejected(lambda: dd.require_handoff(document),
                    "internally inconsistent",
                    "runs claiming PASS while carrying a FAIL check")


def test_an_unresolved_commit_is_not_evidence() -> None:
    """`probe_flake` writes the literal `unknown` when git was unreachable.

    That is a well-formed result FIELD — the declared schema accepts it,
    and `probe_census.require_commit_identity` refuses it BY NAME rather
    than by failing a hex test. Diagnosis evidence needs the identity,
    so the placeholder is malformed input here; a `/deflake`-produced
    record can never carry one, because `deflake._require_commit`
    already refuses to record such a measurement.
    """
    document = handoff_document(result=result_document(
        runs=failing_runs(3), commit="unknown"))
    expect_rejected(lambda: dd.require_handoff(document),
                    "is the placeholder 'unknown'",
                    "a result document carrying the placeholder commit")

    for field in ("commit_sha", "base_sha"):
        document = diagnosis_document()
        document["repair"][field] = "unknown"
        expect_rejected(lambda d=document: evaluate(d),
                        "is the placeholder 'unknown'",
                        f"a repair whose {field} is the placeholder")


def test_commit_identity_is_delegated_rather_than_reimplemented() -> None:
    """One grammar, `probe_census`'s, exactly as `timestamp_utc` is.

    A second local copy could drift into accepting a batch the producer
    and the census both reject. The delegation is asserted directly —
    the helper is called and its `CensusError` is converted — because a
    reimplementation that happened to agree today would satisfy every
    behavioural case above while still being a second grammar.
    """
    calls = []
    original = probe_census.require_commit_identity

    def recording(value, what):
        calls.append((value, what))
        return original(value, what)

    probe_census.require_commit_identity = recording
    try:
        evaluate(diagnosis_document())
    finally:
        probe_census.require_commit_identity = original
    expect(calls, "every commit identity goes through probe_census")
    expect(any(what.endswith("commit_sha") for _v, what in calls),
           f"including the result documents' own: {[w for _v, w in calls]}")

    # And the census's own refusal reaches the caller as a controlled
    # malformed-input rejection, never as an escaping `CensusError`.
    try:
        dd.require_commit("nope", "a field", because="because")
    except dd.HandoffError as error:
        expect("because" in str(error),
               f"the refusal says why the field matters: {error}")
    except probe_census.CensusError:  # pragma: no cover - the bug this pins
        expect(False, "a CensusError escaped instead of a HandoffError")
    else:
        expect(False, "an invalid identity was accepted")


# ==========================================================================
# No retries
# ==========================================================================
def test_a_retry_policy_is_refused() -> None:
    document = handoff_document(inv=deflake_invocation(retries=1))
    expect_rejected(lambda: dd.require_handoff(document),
                    "retry policy", "a handoff measured with retries")


def test_an_absent_retry_policy_is_refused() -> None:
    inv = deflake_invocation()
    del inv["retries"]
    document = handoff_document(inv=inv)
    expect_rejected(lambda: dd.require_handoff(document),
                    "retry policy", "a handoff that states no retry policy")


def test_the_handoff_comes_from_deflake_and_the_batches_from_the_harness() -> None:
    """The three batches do not come from one command.

    `/deflake` calls `probe_flake.measure` IN PROCESS and its CLI has no
    `--probe`, `--runs` or RTS override at all, so requiring a
    `probe_flake.py` argv for the handoff would make a truthful #1436
    record impossible to submit while accepting an argv nobody ran.
    """
    expect(dd.DEFLAKE_LAUNCHER.fixed == {"runs": dd.RUN_COUNT,
                                         "rts_caps": dd.RTS_CAPABILITIES},
           f"/deflake supplies both counts itself: {dd.DEFLAKE_LAUNCHER.fixed}")
    expect(dd.DEFLAKE_LAUNCHER.probe_from_result,
           "and the probe comes from the document its selector produced")

    accepted = dd.require_handoff(handoff_document())
    expect(accepted.probe == PROBE, "a real /deflake record is admitted")

    # Each record keeps its OWN shape and borrows only the other's argv,
    # so what fails is the launcher rule rather than a missing key.
    swapped = handoff_document()
    swapped["invocation"]["argv"] = command(worktree=PRIMARY_WT)[1:]
    expect_rejected(lambda: dd.require_handoff(swapped),
                    "come from deflake.py",
                    "a handoff claiming a probe_flake.py argv")

    document = diagnosis_document()
    document["baseline"]["invocation"]["command"] = [
        "python3", f"{CLEAN_WT}/tools/deflake.py", "--json"]
    expect_rejected(lambda: evaluate(document),
                    "come from probe_flake.py",
                    "a controlled batch claiming a /deflake argv")

    counterfeit = handoff_document()
    counterfeit["invocation"]["argv"] = [
        "/tmp/counterfeit/deflake.py", "--json"]
    expect_rejected(lambda: dd.require_handoff(counterfeit),
                    "the checkout it declares keeps that tool at",
                    "a handoff claiming a counterfeit /deflake")


def test_a_deflake_command_takes_only_its_own_two_options() -> None:
    for extra in (["--probe", PROBE], ["--runs", "10"],
                  ["--rts-caps", "4"], ["--artifact-root", OUTSIDE]):
        document = handoff_document(inv=deflake_invocation(cmd=[
            f"{PRIMARY_WT}/tools/deflake.py", "--json"] + extra))
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "does not accept",
                        f"a /deflake command carrying {extra[0]}")

    # `--json` is a flag, so it must not swallow the next argument.
    document = handoff_document(inv=deflake_invocation(cmd=[
        f"{PRIMARY_WT}/tools/deflake.py",
        "--json", "--result", f"{OUTSIDE}/handoff.json"]))
    dd.require_handoff(document)
    document = handoff_document(inv=deflake_invocation(cmd=[
        f"{PRIMARY_WT}/tools/deflake.py",
        "--json=true", "--result", f"{OUTSIDE}/handoff.json"]))
    expect_rejected(lambda: dd.require_handoff(document),
                    "which is a flag", "a value passed to --json")

    # `--result` is OPTIONAL there: /deflake retains the document beside
    # its artifacts whether or not it is also copied out.
    document = handoff_document(inv=deflake_invocation(cmd=[
        f"{PRIMARY_WT}/tools/deflake.py", "--json"]))
    dd.require_handoff(document)


def test_a_batch_invocation_must_match_the_measurement_it_describes() -> None:
    for label, cmd, fragment in (
            ("another probe", command(probe=OTHER), "describe one measurement"),
            ("five runs", command(runs=5), "describe one measurement"),
            ("eight capabilities", command(rts_caps=8),
             "describe one measurement")):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(cmd=cmd)
        expect_rejected(lambda d=document: evaluate(d), fragment,
                        f"a baseline command claiming {label}")


# ==========================================================================
# Configuration: contents AND absence
# ==========================================================================
def test_an_empty_manifest_is_an_explicit_statement() -> None:
    root = Path(tempfile.mkdtemp(prefix="test_deflake_diagnosis_"))
    try:
        (root / "config").mkdir()
        document = dd.config_manifest(root)
        expect(document["entries"] == [],
               "an absent configuration family is an empty entry list")
        dd.require_manifest(document, "manifest")
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_a_manifest_records_a_digest_per_file() -> None:
    root = Path(tempfile.mkdtemp(prefix="test_deflake_diagnosis_"))
    try:
        (root / "config").mkdir()
        (root / "config" / "save.local.yaml").write_text("autosave: true\n")
        (root / "config" / "video.local.yaml").write_text("vsync: false\n")
        (root / "config" / "ignored.yaml").write_text("not in the family\n")
        document = dd.config_manifest(root)
        paths = [entry["path"] for entry in document["entries"]]
        expect(paths == ["config/save.local.yaml", "config/video.local.yaml"],
               f"only the gitignored family is recorded, sorted; got {paths}")
        expect(all(dd.SHA256_RE.match(entry["sha256"])
                   for entry in document["entries"]),
               "every entry carries a SHA-256 digest")
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_a_manifest_entry_must_name_the_gitignored_family() -> None:
    """Otherwise "identical manifests" can be identical about anything.

    Two documents both listing `../outside-config.local.yaml` agree
    perfectly and establish nothing about the `config/*.local.yaml` state
    the probes actually symlink into their isolated resource roots.
    """
    for relative in ("../outside-config.local.yaml",
                     "/etc/config/save.local.yaml",
                     "config/nested/save.local.yaml",
                     "config/save.yaml",
                     "save.local.yaml",
                     "config/../config/save.local.yaml"):
        expect_rejected(lambda r=relative: dd.require_manifest(
            manifest([(r, "c" * 64)]), "manifest"),
            "gitignored", f"a manifest entry naming {relative!r}")

    for relative in ("config/save.local.yaml", "config/video.local.yaml",
                     "config/keybinds.local.yaml",
                     "config/notifications.local.yaml"):
        dd.require_manifest(manifest([(relative, "c" * 64)]), "manifest")

    # The real generator only ever produces members of the family.
    root = Path(tempfile.mkdtemp(prefix="test_deflake_diagnosis_"))
    try:
        (root / "config").mkdir()
        (root / "config" / "save.local.yaml").write_text("autosave: true\n")
        dd.require_manifest(dd.config_manifest(root), "generated manifest")
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_both_comparison_worktrees_are_source_clean() -> None:
    """The clean side needs the attestation as much as the repair side.

    Its recorded SHA cannot reveal an uncommitted change, and "the clean
    comparison worktree must remain unmodified" is a contract about its
    SOURCE — the gitignored configuration state it must also reproduce is
    recorded separately in its own manifest.
    """
    for section in ("baseline", "verification"):
        for value in (None, False, "yes"):
            document = diagnosis_document()
            if value is None:
                del document[section]["source_clean"]
            else:
                document[section]["source_clean"] = value
            expect_rejected(lambda d=document: evaluate(d),
                            "not recorded as source-clean",
                            f"a {section} recorded as {value!r}")


def test_a_batch_may_not_write_into_the_other_declared_worktree() -> None:
    """Both declarations are collected BEFORE either batch is validated.

    That is what still holds once the comparison worktrees have been
    removed — which the workflow requires — and neither appears in
    `worktree_paths()` any more. Checked here with NO registered
    worktrees at all, which is exactly the post-cleanup state.
    """
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(
        cmd=command(result=f"{REPAIR_WT}/baseline.json",
                    artifacts=f"{OUTSIDE}/artifacts", worktree=CLEAN_WT))
    expect_rejected(lambda: evaluate(document, worktrees=()),
                    "inside the working tree",
                    "a baseline writing into the repair worktree")

    # The layout moves as a whole, because topology pins every derived
    # path to the artifact root — so the reachable case is a root inside
    # the OTHER comparison state.
    other = diagnosis_document(handoff=handoff_document(acceptable=1))
    root = f"{CLEAN_WT}/artifacts"
    other["verification"]["result"] = verification_result(
        runs=failing_runs(1), artifact_root=root)
    other["verification"]["invocation"] = invocation(
        cmd=command(result=f"{OUTSIDE}/verify.json", artifacts=root,
                    worktree=REPAIR_WT),
        directory=REPAIR_WT, ports=[9201])
    expect_rejected(lambda: evaluate(other, worktrees=()),
                    "inside the working tree",
                    "a verification retaining logs in the clean worktree")


def test_a_command_must_agree_with_the_result_it_produced() -> None:
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(
        cmd=command(probe=OTHER))
    expect_rejected(lambda: evaluate(document),
                    "describe one measurement",
                    "a command naming a probe its result did not measure")


def test_a_manifest_with_no_entries_key_is_refused() -> None:
    """Absence has to be ASSERTED, not inferred from an omitted key."""
    expect_rejected(lambda: dd.require_manifest(
        {"schema": dd.MANIFEST_SCHEMA}, "manifest"),
        "record an empty list", "a manifest that omits `entries`")


def test_absence_on_one_side_is_a_difference() -> None:
    empty = manifest()
    present = manifest([("config/save.local.yaml", "c" * 64)])
    problems = dd.manifest_differences(empty, present, left_name="left",
                                       right_name="right")
    expect(len(problems) == 1 and "absent from left" in problems[0],
           f"an extra file on one side is a difference; got {problems}")
    problems = dd.manifest_differences(present, empty, left_name="left",
                                       right_name="right")
    expect(len(problems) == 1 and "absent from right" in problems[0],
           f"and so is a missing one; got {problems}")
    expect(dd.manifest_differences(empty, manifest(), left_name="left",
                                   right_name="right") == [],
           "two confirmed-absent manifests agree")


def test_a_digest_difference_is_a_difference() -> None:
    a = manifest([("config/save.local.yaml", "c" * 64)])
    b = manifest([("config/save.local.yaml", "d" * 64)])
    problems = dd.manifest_differences(a, b, left_name="left",
                                       right_name="right")
    expect(len(problems) == 1 and "differs" in problems[0],
           f"different contents are a difference; got {problems}")


def test_a_baseline_under_another_configuration_is_not_the_condition() -> None:
    document = diagnosis_document()
    document["baseline"]["configuration"] = manifest(
        [("config/save.local.yaml", "e" * 64)])
    expect_refused(lambda: evaluate(document),
                   "did not reproduce the handoff's configuration state",
                   "a baseline under another configuration")


def test_the_two_comparison_worktrees_must_agree() -> None:
    document = diagnosis_document()
    document["verification"]["configuration"] = manifest(
        [("config/keybinds.local.yaml", "f" * 64)])
    expect_refused(lambda: evaluate(document),
                   "do not hold the same configuration state",
                   "comparison worktrees that disagree")


# ==========================================================================
# Controlled reproduction
# ==========================================================================
def test_a_diagnosis_without_a_controlled_baseline_is_refused() -> None:
    document = diagnosis_document()
    del document["baseline"]
    expect_rejected(lambda: evaluate(document),
                    "carries no controlled pre-fix baseline",
                    "a diagnosis with no baseline")


def test_a_baseline_at_or_below_x_cannot_support_a_repair() -> None:
    """A baseline that reproduced the target but stayed within tolerance.

    The target IS observed here, so the only rule left to refuse this is
    "the controlled baseline must exceed X" — a spotless baseline would
    be refused by the target rule first and would prove nothing about
    the arithmetic.

    `abort=False` is load-bearing: an aborting run leaves the later
    target MISSING, which since the 2026-08-24 correction qualifies for
    repair on its own and would refuse this for the OTHER reason. A
    non-aborting run FAILs its target and emits the rest, so the
    aggregate rule is genuinely the only one left.
    """
    document = diagnosis_document(handoff=handoff_document(acceptable=1))
    document["baseline"]["result"] = result_document(
        runs=failing_runs(1, abort=False))
    expect_refused(lambda: evaluate(document), "cannot-reproduce",
                   "a baseline within tolerance")

    spotless = diagnosis_document()
    spotless["baseline"]["result"] = result_document()
    expect_refused(lambda: evaluate(spotless), "cannot-reproduce",
                   "a spotless baseline")


def test_a_baseline_that_never_hits_the_target_cannot_support_a_repair() -> None:
    """Over tolerance, but failing somewhere else entirely.

    The target is the FIRST declared check and the baseline aborts at the
    second, so the target is PASS in every run — an abort at an earlier
    check would have made it MISSING, which is a non-PASS observation and
    would legitimately count as reproducing the pattern.
    """
    handoff = handoff_document(result=result_document(
        runs=failing_runs(3, cid="alpha", abort=False)))
    expect(handoff["targets"] == ["alpha"],
           f"a non-aborting failure implicates only itself; got "
           f"{handoff['targets']}")
    document = diagnosis_document(handoff=handoff)
    document["baseline"]["result"] = result_document(
        runs=failing_runs(4, cid="gamma", abort=False))
    expect_refused(lambda: evaluate(document), "cannot-reproduce",
                   "a baseline that reproduced another failure")


def test_the_baseline_must_be_measured_at_the_handoffs_own_commit() -> None:
    """One common SHA, or the two states are not a comparison."""
    document = diagnosis_document()
    document["baseline"]["result"] = result_document(
        commit="d" * 40, runs=failing_runs(4))
    expect_rejected(lambda: evaluate(document),
                    "recreate BOTH states on one new common SHA",
                    "a baseline measured at another commit")


def test_the_repair_must_be_cut_from_that_same_commit() -> None:
    document = diagnosis_document()
    del document["repair"]["base_sha"]
    expect_rejected(lambda: evaluate(document),
                    "must be a commit hash string",
                    "a repair whose lineage is unstated")

    document = diagnosis_document()
    document["repair"]["base_sha"] = "e" * 40
    expect_rejected(lambda: evaluate(document),
                    "share one common SHA or they are not a comparison",
                    "a repair cut from another commit")


def test_a_relative_destination_is_resolved_before_it_is_judged() -> None:
    """`probe_flake.write_result` opens `--result` relative to its cwd.

    So `results/verification.json` from inside the repair worktree lands
    IN that worktree while matching no absolute registered path at all.
    The recorded invocation directory is what makes a relative
    destination mean something, and it is joined on before containment
    is decided.
    """
    trees = [REPAIR_WT]
    for relative in ("results/verification.json",
                     "./results/verification.json",
                     "../deflake-role/verification.json"):
        document = diagnosis_document()
        document["verification"]["invocation"] = invocation(
            cmd=command(result=relative, artifacts=VERIFY_ARTIFACTS,
                        worktree=REPAIR_WT),
            directory=REPAIR_WT, ports=[9201])
        expect_rejected(lambda d=document: evaluate(d, worktrees=trees),
                        "inside the working tree",
                        f"a --result of {relative!r}")

    outside = diagnosis_document()
    outside["verification"]["invocation"] = invocation(
        cmd=command(result="../evidence/verification.json",
                    artifacts=VERIFY_ARTIFACTS,
                    worktree=REPAIR_WT),
        directory=REPAIR_WT, ports=[9201])
    outcome = evaluate(outside, worktrees=trees)
    expect(outcome.route == dd.ROUTE_REPAIR,
           "a relative path that really does leave the worktree is fine")


def test_both_batches_must_stay_outside_every_worktree() -> None:
    trees = [REPAIR_WT]
    document = diagnosis_document()
    document["verification"]["invocation"] = invocation(
        cmd=command(result="/tmp/deflake-role/verify.json",
                    artifacts=VERIFY_ARTIFACTS,
                    worktree=REPAIR_WT),
        directory=REPAIR_WT, ports=[9201])
    expect_rejected(lambda: evaluate(document, worktrees=trees),
                    "inside the working tree",
                    "a result document written into a worktree")


def test_the_two_batches_may_not_share_a_destination() -> None:
    document = diagnosis_document()
    document["verification"]["invocation"] = invocation(
        cmd=command(worktree=REPAIR_WT), directory=REPAIR_WT, ports=[9201])
    document["verification"]["result"] = verification_result(
        artifact_root=f"{OUTSIDE}/artifacts")
    expect_refused(lambda: evaluate(document), "both batches wrote to",
                   "two batches sharing one result path")


def test_a_repair_without_a_verification_batch_is_refused() -> None:
    """A repair is accepted only against a fresh batch, never on the baseline."""
    for route in (dd.ROUTE_REPAIR, dd.ROUTE_PARTIAL_IMPROVEMENT):
        document = diagnosis_document(route=route)
        document.pop("verification")
        expect_rejected(lambda d=document: evaluate(d),
                        "requires a verification batch",
                        f"a {route} route with no verification")


def test_the_two_batches_may_share_a_root_but_not_an_invocation() -> None:
    """`--artifact-root` is optional, so a shared ROOT is legitimate.

    What no two invocations can share is the directory beneath it:
    `new_invocation_dir` creates a fresh collision-free one per
    invocation, stamped with the time, the pid and a uuid. Checking only
    the COMMAND's destinations let both batches omit `--artifact-root`,
    point at one invocation directory, and keep distinct `--result`
    paths — every per-batch rule passing while the verification reported
    the baseline's artifacts as its own.
    """
    def defaulted(section, tree, result):
        section["invocation"] = invocation(
            cmd=["python3", f"{tree}/tools/probe_flake.py", "--probe", PROBE,
                 "--runs", str(dd.RUN_COUNT), "--rts-caps",
                 str(dd.RTS_CAPABILITIES), "--result", result],
            directory=tree)

    document = diagnosis_document()
    defaulted(document["baseline"], CLEAN_WT, f"{OUTSIDE}/baseline.json")
    defaulted(document["verification"], REPAIR_WT, f"{OUTSIDE}/verify.json")
    shared_root = f"{OUTSIDE}/defaulted"
    document["baseline"]["result"] = result_document(
        runs=failing_runs(4), artifact_root=shared_root)
    document["verification"]["result"] = verification_result(
        artifact_root=shared_root)
    expect(document["baseline"]["result"]["invocation_dir"]
           == document["verification"]["result"]["invocation_dir"],
           "the fixture really does reuse one invocation directory")
    expect_refused(lambda: evaluate(document),
                   "creates a fresh one per invocation",
                   "two batches reporting one invocation directory")

    # The same shared root with distinct invocation directories is fine.
    document["verification"]["result"]["invocation_dir"] = (
        f"{shared_root}/{PROBE}-20260822T090000Z-5150-beefcafe")
    document["verification"]["result"]["runs"] = [
        dict(run) for run in document["verification"]["result"]["runs"]]
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"a shared artifact root is legitimate; got {outcome.route}")


def test_neither_batch_may_write_into_the_others_artifacts() -> None:
    """Distinct paths are not isolation if one sits inside the other.

    A verification `--result` pointing at the baseline's retained
    `run-001/events.jsonl` is a different path from anything the baseline
    wrote to, and it overwrites the very evidence the comparison is made
    of.
    """
    document = diagnosis_document()
    inside = (f"{document['baseline']['result']['invocation_dir']}"
              f"/run-001/events.jsonl")
    document["verification"]["invocation"] = invocation(
        cmd=command(result=inside, artifacts=VERIFY_ARTIFACTS,
                    worktree=REPAIR_WT),
        directory=REPAIR_WT)
    expect_refused(lambda: evaluate(document),
                   "invocation directory",
                   "a verification writing into the baseline's artifacts")

    # And the other direction: the baseline reporting a retained artifact
    # inside the verification's invocation directory.
    other = diagnosis_document(handoff=handoff_document(acceptable=1))
    other["verification"]["result"] = verification_result(
        runs=failing_runs(1, abort=False))
    victim = other["verification"]["result"]["invocation_dir"]
    failing = next(run for run in other["baseline"]["result"]["runs"]
                   if run["outcome"] != probe_flake.RUN_PASS)
    moved = f"{victim}/run-{failing['index']:03d}"
    other["baseline"]["result"]["retained_artifacts"] = [
        moved if path == failing["artifact_dir"] else path
        for path in other["baseline"]["result"]["retained_artifacts"]]
    failing["artifact_dir"] = moved
    expect_rejected(lambda: evaluate(other),
                    "evidence from somewhere other than this measurement",
                    "a baseline retaining artifacts in the verification's tree")


def test_a_run_sequence_is_the_one_the_loop_emits() -> None:
    """`measure` runs `range(1, runs + 1)`, one record per index.

    Ten records all numbered `1` is one run replayed ten times — and
    every other rule reads a run's index, so leaving the sequence
    unchecked let a forged layout satisfy them all against one number.
    """
    for label, renumber in (
            ("all the same", lambda runs: [1] * len(runs)),
            ("a repeat", lambda runs: [1, 1] + list(range(3, len(runs) + 1))),
            ("a skip", lambda runs: [1] + list(range(3, len(runs) + 2))),
            ("reordered", lambda runs: list(range(len(runs), 0, -1))),
            ("zero-based", lambda runs: list(range(0, len(runs)))),
    ):
        document = handoff_document()
        runs = document["result"]["runs"]
        for run, index in zip(runs, renumber(runs)):
            run["index"] = index
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "numbers its runs",
                        f"a batch whose indices are {label}")

    broken = handoff_document(result=result_document(
        runs=failing_runs(2), harness_error=True))
    broken["result"]["error_run"]["index"] = 1
    expect_rejected(lambda: dd.require_handoff(broken),
                    "the one after the last completed one",
                    "a harness-error run numbered before the completed ones")


def test_the_two_batches_may_not_share_a_worktree() -> None:
    document = diagnosis_document()
    relocate_section(document["verification"], CLEAN_WT)
    expect_refused(lambda: evaluate(document), "not two separate states",
                   "a verification run in the clean comparison worktree")

    for label, declared in (("a trailing dot", f"{CLEAN_WT}/."),
                            ("a redundant step", f"{CLEAN_WT}/sub/..")):
        document = diagnosis_document()
        relocate_section(document["verification"], declared)
        expect_refused(lambda d=document: evaluate(d),
                       "not two separate states",
                       f"the same worktree spelled with {label}")

    nested = diagnosis_document()
    relocate_section(nested["verification"], f"{CLEAN_WT}/nested")
    expect_refused(lambda: evaluate(nested), "not two separate states",
                   "a repair worktree nested inside the clean one")


def test_a_section_must_measure_in_the_worktree_it_declares() -> None:
    document = diagnosis_document()
    document["verification"]["invocation"]["directory"] = "/tmp/somewhere-else"
    expect_rejected(lambda: evaluate(document),
                    "measures somewhere other than the worktree it names",
                    "a section whose invocation ran elsewhere")


# ==========================================================================
# Same-environment verification
# ==========================================================================
def test_destinations_and_ports_may_differ_and_nothing_else() -> None:
    outcome = evaluate(diagnosis_document())
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"different destinations and ports are fine; got {outcome.route}")

    # Changing only the COMMAND makes the record incoherent — the batch
    # says one thing and the document it produced says another — so it is
    # rejected before comparability is even asked about.
    for label, cmd in (
            ("run count", command(runs=20, result=f"{OUTSIDE}/verify.json",
                                  artifacts=VERIFY_ARTIFACTS,
                                  worktree=REPAIR_WT)),
            ("capabilities", command(rts_caps=8,
                                     result=f"{OUTSIDE}/verify.json",
                                     artifacts=VERIFY_ARTIFACTS,
                                     worktree=REPAIR_WT)),
    ):
        document = diagnosis_document()
        document["verification"]["invocation"] = invocation(
            cmd=cmd, directory=REPAIR_WT, ports=[9201])
        expect_rejected(lambda d=document: evaluate(d),
                        "describe one measurement",
                        f"a verification whose command changed the {label}")


def test_two_commands_that_agree_with_each_other_are_not_the_contract() -> None:
    """The comparison is to the contract, not only to the other batch.

    Both commands claiming twenty runs at eight capabilities compare
    EQUAL to each other. Each is bound to its own result document, so
    matching them needs result documents saying twenty and eight too —
    and those are not measurements this lab's policy is stated on, which
    routes them to #1439 instead of a pull request.
    """
    document = diagnosis_document(route=dd.ROUTE_PARTIAL_IMPROVEMENT)
    document["route"] = dd.ROUTE_CANNOT_REPRODUCE
    for key, dest, tree in (("baseline", f"{OUTSIDE}/baseline.json", CLEAN_WT),
                            ("verification", f"{OUTSIDE}/verify.json",
                             REPAIR_WT)):
        artifacts = (f"{OUTSIDE}/artifacts" if key == "baseline"
                     else VERIFY_ARTIFACTS)
        document[key]["invocation"] = invocation(
            cmd=command(runs=20, rts_caps=8, result=dest,
                        artifacts=artifacts, worktree=tree),
            directory=tree, ports=[9101])
    document["baseline"]["result"] = result_document(
        runs=failing_runs(4), requested=20, rts_caps=8, command_runs=20)
    document["verification"]["result"] = verification_result(
        requested=20, rts_caps=8, command_runs=20)
    expect(dd.invocation_differences(document["baseline"]["invocation"],
                                     document["verification"]["invocation"])
           == [], "the two commands really do compare equal to each other")
    # And that is not enough: the conditions travel the whole chain, so
    # both are measured against the HANDOFF's contract, not each other's.
    expect_refused(lambda: evaluate(document),
                   "did not replay the conditions the handoff was measured",
                   "two batches agreeing with each other but not the handoff")

    repair = copy.deepcopy(document)
    repair["route"] = dd.ROUTE_REPAIR
    expect_refused(lambda: evaluate(repair),
                   "did not replay the conditions the handoff was measured",
                   "a repair declared over two agreeing non-measurements")


def test_only_the_real_harness_options_are_accepted() -> None:
    """A plausible option the shipped CLI does not have is not a condition.

    `probe_flake.main` exposes no `--timeout`, so a pair of commands both
    carrying `--timeout 60` would compare EQUAL and pass same-environment
    validation while describing a measurement neither batch could have
    run. Every option is checked against the real surface instead.
    """
    for extra in (["--timeout", "60"], ["--start-port", "9500"],
                  ["--retries", "2"], ["--jobs", "4"]):
        document = diagnosis_document()
        document["verification"]["invocation"] = invocation(
            cmd=command(result=f"{OUTSIDE}/verify.json",
                        artifacts=VERIFY_ARTIFACTS,
                        worktree=REPAIR_WT) + extra,
            directory=REPAIR_WT, ports=[9201])
        expect_rejected(lambda d=document: evaluate(d),
                        "does not accept",
                        f"a command carrying {extra[0]}")


def test_an_integer_option_uses_the_harnesss_own_grammar() -> None:
    """`--runs 10.0` is numerically ten and argparse would refuse it.

    `probe_flake.main` declares both as `type=int`, so a float spelling
    exits before the harness measures anything — while a comparison that
    parsed it as a number would let the fabricated command compare equal
    to a real one.
    """
    for token in ("10.0", "1e1", " 10.5", "ten", "", "0x0a"):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(
            cmd=command() [:4] + ["--runs", token] + [
                "--rts-caps", str(dd.RTS_CAPABILITIES),
                "--result", f"{OUTSIDE}/baseline.json",
                "--artifact-root", f"{OUTSIDE}/artifacts"])
        expect_rejected(lambda d=document: evaluate(d),
                        "must be an integer",
                        f"a --runs of {token!r}")

    for token in ("4.0", "four"):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(
            cmd=command() [:6] + ["--rts-caps", token] + [
                "--result", f"{OUTSIDE}/baseline.json",
                "--artifact-root", f"{OUTSIDE}/artifacts"])
        expect_rejected(lambda d=document: evaluate(d),
                        "must be an integer",
                        f"a --rts-caps of {token!r}")

    # What `int()` does accept, this accepts, because that is argparse's
    # own grammar and nothing narrower.
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(
        cmd=command() [:4] + ["--runs", f" {dd.RUN_COUNT} "] + [
            "--rts-caps", str(dd.RTS_CAPABILITIES),
            "--result", f"{OUTSIDE}/baseline.json",
            "--artifact-root", f"{OUTSIDE}/artifacts"])
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"a spelling argparse accepts is accepted; got {outcome.route}")


def test_a_command_that_wrote_no_result_document_produced_no_evidence() -> None:
    """`probe_flake.main` writes the document only `if args.result`."""
    for section in ("baseline", "verification"):
        document = diagnosis_document()
        cmd = [token for token in command()]
        index = cmd.index("--result")
        del cmd[index:index + 2]
        tree = CLEAN_WT if section == "baseline" else REPAIR_WT
        cmd = [f"{tree}/tools/probe_flake.py" if token.endswith(
            "/tools/probe_flake.py") else token for token in cmd]
        document[section]["invocation"] = invocation(
            cmd=cmd, directory=tree)
        expect_rejected(lambda d=document: evaluate(d), "names no --result",
                        f"a {section} command with no result destination")

    # Not the handoff: `/deflake` retains the document beside its
    # artifacts either way, so its `--result` is genuinely optional.


def test_the_artifact_layout_is_the_one_the_harness_creates() -> None:
    """Three recorded values determine the whole layout, and nothing else.

    `new_invocation_dir` puts the invocation directory directly under the
    artifact root and names it after the probe; every run directory is
    `invocation_dir / f"run-{index:03d}"`. Containment alone let a batch
    swap a failed run's directory for an unrelated external path and keep
    `repair-pr`.
    """
    nested = handoff_document()
    result = nested["result"]
    result["invocation_dir"] = f"{OUTSIDE}/artifacts/deeper/{PROBE}-x-1-a"
    expect_rejected(lambda: dd.require_handoff(nested),
                    "DIRECT child of the root",
                    "an invocation directory two levels under the root")

    for label, name in (
            ("not named after the probe", f"{OTHER}-20260821T120000Z-1-abcdef12"),
            ("a name the harness never generates", "invocation"),
            ("a name with no uuid", f"{PROBE}-20260821T120000Z-1"),
            ("a name with a bad stamp", f"{PROBE}-2026-08-21-1-abcdef12"),
            ("a name with non-hex", f"{PROBE}-20260821T120000Z-1-abcdefgh")):
        misnamed = handoff_document()
        misnamed["result"]["invocation_dir"] = f"{OUTSIDE}/artifacts/{name}"
        expect_rejected(lambda d=misnamed: dd.require_handoff(d),
                        "not a directory this measurement created",
                        f"an invocation directory {label}")

    for label, key in (("an artifact root", "artifact_root"),
                       ("an invocation directory", "invocation_dir")):
        relative = handoff_document()
        relative["result"][key] = "artifacts/relative"
        expect_rejected(lambda d=relative: dd.require_handoff(d),
                        "every path a real result document carries is "
                        "absolute",
                        f"{label} recorded as a relative path")

    for label, replacement in (
            ("an unrelated external path", f"{OUTSIDE}/elsewhere/run-001"),
            ("another run's directory", None),
            ("a sibling of the invocation directory", None)):
        document = handoff_document()
        result = document["result"]
        failing = next(run for run in result["runs"]
                       if run["outcome"] != probe_flake.RUN_PASS)
        if replacement is None:
            other = next(run for run in result["runs"]
                         if run["outcome"] != probe_flake.RUN_PASS
                         and run["index"] != failing["index"])
            replacement = (other["artifact_dir"] if label.startswith("another")
                           else str(Path(result["invocation_dir"]).parent
                                    / f"run-{failing['index']:03d}"))
        result["retained_artifacts"] = [
            replacement if path == failing["artifact_dir"] else path
            for path in result["retained_artifacts"]]
        failing["artifact_dir"] = replacement
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "evidence from somewhere other than this measurement",
                        f"a run directory replaced by {label}")

    broken = handoff_document(result=result_document(
        runs=failing_runs(2), harness_error=True))
    broken["result"]["error_run"]["artifact_dir"] = f"{OUTSIDE}/elsewhere/run"
    broken["result"]["retained_artifacts"][-1] = f"{OUTSIDE}/elsewhere/run"
    expect_rejected(lambda: dd.require_handoff(broken),
                    "evidence from somewhere other than this measurement",
                    "a harness-error run directory somewhere else")


def test_only_a_python_three_interpreter_is_accepted() -> None:
    """These are Python 3 programs; `python2` is a SyntaxError, not a run.

    Bare `python` is refused for a different reason: it is whichever of
    the two that machine happens to mean, which a document cannot settle.
    A token that is not a version-qualified `python3` at all — `pypy`,
    `python4x`, a bare `sh` — never names the interpreter these
    documents quote.
    """
    for program in ("python", "python2", "python2.7", "pypy", "python4x"):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(
            cmd=[program] + command()[1:])
        expect_rejected(lambda d=document: evaluate(d),
                        "is not a Python 3 interpreter token",
                        f"a command run by {program!r}")

    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(
        cmd=["python3"] + command()[1:])
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"'python3' is the recorded spelling; got {outcome.route}")


def test_a_version_qualified_interpreter_names_the_same_program() -> None:
    """`python3.12` is `python3` said precisely, not a different tool.

    A machine with several Python 3 installations spells the one it
    means with its version, and that command runs exactly the program a
    correctly-pointed `python3` would have run. Refusing the spelling
    outright would reject a truthful record for naming its interpreter
    more exactly than the bare form does.

    Both controlled commands are changed together, because a single
    altered record is already caught by the same-environment comparison
    — which would make this rule look enforced when only that one was.
    """
    for program in ("python3.10", "python3.12", "python3.10.4",
                    f"python3.{dd.INTERPRETER_MINOR_FLOOR}"):
        document = diagnosis_document()
        for batch in ("baseline", "verification"):
            recorded = document[batch]["invocation"]
            recorded["command"] = [program] + recorded["command"][1:]
        outcome = evaluate(document)
        expect(outcome.route == dd.ROUTE_REPAIR,
               f"{program!r} is at or above the floor; got {outcome.route}")


def test_an_interpreter_below_the_syntax_floor_could_not_have_run_it() -> None:
    """`python3.9` names a version that cannot run the program quoting it.

    The floor is DERIVED, not invented: the shipped tools annotate with
    `X | None`, and while `from __future__ import annotations` defers the
    ones in signatures, nothing defers a type evaluated at runtime — so
    3.10 is where these sources stop being runnable rather than merely
    parseable. A record naming an older interpreter describes a run that
    could not have produced the document.
    """
    for program in ("python3.0", "python3.6", "python3.9", "python3.9.18"):
        document = diagnosis_document()
        for batch in ("baseline", "verification"):
            recorded = document[batch]["invocation"]
            recorded["command"] = [program] + recorded["command"][1:]
        expect_rejected(lambda d=document: evaluate(d),
                        "below this lab's 3.10 syntax floor",
                        f"every controlled command run by {program!r}")

    expect(dd.INTERPRETER_MINOR_FLOOR == 10,
           f"the floor is 3.10, the version `X | None` needs; got "
           f"3.{dd.INTERPRETER_MINOR_FLOOR}")


def test_a_malformed_version_token_is_not_an_interpreter() -> None:
    """A version is a dotted run of digits, with no second spelling.

    `python3.010` and `python3.10` would name one interpreter two ways,
    and diagnosis evidence gets one canonical spelling per interpreter
    for the same reason a duplicated option is refused. `python3.` and
    `python3.x` name no version at all.
    """
    for program in ("python3.", "python3.x", "python3..10", "python3.010",
                    "python3.10.", "python3.10.04", "python3.-1"):
        document = diagnosis_document()
        for batch in ("baseline", "verification"):
            recorded = document[batch]["invocation"]
            recorded["command"] = [program] + recorded["command"][1:]
        expect_rejected(lambda d=document: evaluate(d),
                        "is not a Python 3 interpreter token",
                        f"every controlled command run by {program!r}")


def test_a_path_qualified_interpreter_is_refused_whatever_it_names() -> None:
    """The token is a bare name resolved through `PATH`.

    A document cannot show which binary sits at an arbitrary path, so
    the rejection is about the SHAPE of the token and does not depend on
    the version it appears to name — `/usr/bin/python3.12` is refused
    exactly as `/tmp/counterfeit/python3` is.
    """
    for program in ("/usr/bin/python3", "/usr/bin/python3.12",
                    "/tmp/counterfeit/python3", "./python3",
                    "../bin/python3.11"):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(
            cmd=[program] + command()[1:])
        expect_rejected(lambda d=document: evaluate(d),
                        "runs the interpreter by path",
                        f"a command run by {program!r}")


def test_a_handoff_argv_carries_no_interpreter_token_at_all() -> None:
    """`sys.argv[0]` is the SCRIPT, whatever the interpreter was called.

    Putting a token there is refused as the wrong FORM, which is a
    stronger statement than refusing its version — and it holds for an
    accepted spelling exactly as for a below-floor one.
    """
    for program in ("python3", "python3.12", "python3.9"):
        document = handoff_document()
        document["invocation"]["argv"] = (
            [program] + document["invocation"]["argv"])
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "Python never puts the interpreter there",
                        f"a handoff argv prefixed with {program!r}")


def test_an_identity_with_a_trailing_newline_is_not_an_identity() -> None:
    """`re.match` with `$` is not full-string validation.

    `$` matches immediately before a final newline, so a 40-character
    hash spelled `"<sha>\n"` satisfies a `re.match` anchored with `$`
    while being no Git SHA at all — which is why
    `probe_census.require_commit_identity` compares the whole string.
    Every identity in the document is mutated together,
    because a document that spells one that way spells all of them that
    way — and a single altered field would be caught by the equality
    comparisons instead, hiding whether this rule is enforced.
    """
    document = diagnosis_document()
    document["handoff"]["result"]["commit_sha"] += "\n"
    document["baseline"]["result"]["commit_sha"] += "\n"
    expect_rejected(lambda d=document: evaluate(d),
                    "must be 40 lowercase hex characters",
                    "a measurement commit with a trailing newline")

    document = diagnosis_document()
    document["repair"]["commit_sha"] += "\n"
    expect_rejected(lambda d=document: evaluate(d),
                    "must be 40 lowercase hex characters",
                    "a repair commit with a trailing newline")

    document = diagnosis_document()
    document["repair"]["base_sha"] += "\n"
    expect_rejected(lambda d=document: evaluate(d),
                    "must be 40 lowercase hex characters",
                    "a repair base commit with a trailing newline")


def test_a_config_digest_with_a_trailing_newline_is_refused() -> None:
    """The same `$`-before-newline hole, on the manifest's digests.

    Driven through `require_manifest` directly, the way every sibling
    manifest rule here is: the default fixture manifest is EMPTY, which
    is this lab's expected default rather than an edge case.
    """
    expect_rejected(
        lambda: dd.require_manifest(
            manifest([("config/video.local.yaml", "c" * 64 + "\n")]),
            "manifest"),
        "SHA-256 digest",
        "a configuration digest with a trailing newline")

    # The same digest without it is the accepted spelling.
    dd.require_manifest(
        manifest([("config/video.local.yaml", "c" * 64)]), "manifest")


def test_a_check_identifier_with_a_trailing_newline_is_refused() -> None:
    """And on the protocol identifiers, for the same reason.

    Renamed in EVERY place the identifier appears — the declaration, each
    run's map, the tally, and the target list — because mutating one
    alone is caught first by the census's own `check_counts` consistency
    rule, which would make this rule look enforced when it was not.
    """
    document = handoff_document()
    original = document["result"]["checks"][0]["id"]
    spelled = original + "\n"
    for entry in document["result"]["checks"]:
        if entry["id"] == original:
            entry["id"] = spelled
    for run in document["result"]["runs"]:
        if original in run["checks"]:
            run["checks"][spelled] = run["checks"].pop(original)
    counts = document["result"]["check_counts"]
    if original in counts:
        counts[spelled] = counts.pop(original)
    document["targets"] = [(spelled if cid == original else cid)
                           for cid in document["targets"]]
    expect_rejected(lambda: dd.require_handoff(document),
                    "no stable identifier",
                    "a check identifier with a trailing newline")


def test_the_handoff_comes_from_the_primary_checkout() -> None:
    """A path cannot assert that it is a checkout, so one must be named.

    `/deflake` runs in the primary checkout — it is the step BEFORE this
    workflow creates its comparison worktrees, and it claims a probe and
    writes the census from there.
    """
    for label, elsewhere in (
            ("an invented root", "/tmp/not-a-synarchy-checkout"),
            ("the clean comparison worktree", CLEAN_WT),
            ("the repair worktree", REPAIR_WT)):
        document = diagnosis_document()
        document["handoff"]["invocation"] = deflake_invocation(
            cmd=[f"{elsewhere}/tools/deflake.py", "--json",
                 "--result", f"{OUTSIDE}/handoff.json"],
            directory=elsewhere)
        expect_rejected(lambda d=document: evaluate(d),
                        "is not the primary checkout",
                        f"a handoff claiming to have run in {label}")

    # Spelled differently, the same checkout is still the same checkout.
    document = diagnosis_document()
    document["handoff"]["invocation"] = deflake_invocation(
        cmd=[f"{PRIMARY_WT}/./tools/deflake.py", "--json",
             "--result", f"{OUTSIDE}/handoff.json"],
        directory=f"{PRIMARY_WT}/.")
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"a canonically equal spelling is accepted; got {outcome.route}")

    expect(dd.primary_checkout() == dd.worktree_paths()[0],
           "and the primary checkout is the head of the registered list")


def test_a_result_path_must_be_spelled_the_way_resolve_spells_it() -> None:
    """`check_artifact_root` resolves, so a real path has no `.` or `..`.

    Normalising a supplied path before comparing it would accept
    `/tmp/evidence/forged/../artifacts/…`, which the harness could not
    have written and which points somewhere else entirely if any
    component is a symlink.
    """
    # Written straight onto the document: `Path` collapses `.`, doubled
    # separators and a trailing slash as it is CONSTRUCTED, so a fixture
    # that went through it could not produce these spellings — which is
    # also why no real result document carries one.
    for label, root in (
            ("a parent step", f"{OUTSIDE}/forged/../artifacts"),
            ("a self step", f"{OUTSIDE}/./artifacts"),
            ("a doubled separator", f"{OUTSIDE}//artifacts"),
            ("a trailing slash", f"{OUTSIDE}/artifacts/")):
        document = handoff_document()
        document["result"]["artifact_root"] = root
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "not the spelling `Path.resolve` produces",
                        f"an artifact root with {label}")

    document = handoff_document()
    failing = next(run for run in document["result"]["runs"]
                   if run["outcome"] != probe_flake.RUN_PASS)
    forged = failing["artifact_dir"].replace("/run-", "/./run-")
    document["result"]["retained_artifacts"] = [
        forged if path == failing["artifact_dir"] else path
        for path in document["result"]["retained_artifacts"]]
    failing["artifact_dir"] = forged
    expect_rejected(lambda: dd.require_handoff(document),
                    "not the spelling `Path.resolve` produces",
                    "a run directory with a self step")


def test_an_unresolved_symlink_is_not_the_serialized_path() -> None:
    """`check_artifact_root` RESOLVES, so a real path has none left.

    Driven against a real symlink rather than a hard-coded platform
    quirk, so it means the same thing on a host where `/tmp` is a real
    directory as on one where it is a link to `/private/tmp`.
    """
    root = Path(tempfile.mkdtemp(prefix="test_deflake_diagnosis_")).resolve()
    try:
        (root / "real").mkdir()
        (root / "link").symlink_to(root / "real")
        document = handoff_document()
        document["result"]["artifact_root"] = str(root / "link" / "artifacts")
        expect_rejected(lambda: dd.require_handoff(document),
                        "not the spelling `Path.resolve` produces",
                        "an artifact root reached through a symlink")

        resolved = handoff_document(result=result_document(
            runs=failing_runs(3),
            artifact_root=str(root / "real" / "artifacts")))
        dd.require_handoff(resolved)
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_a_relative_script_resolves_from_the_directory_it_ran_in() -> None:
    """Python resolves a relative script path from the CWD, not the checkout.

    So an invocation in a SUBDIRECTORY of the declared worktree can write
    `tools/probe_flake.py` and mean a counterfeit nested beside it —
    which resolving against the checkout would have compared to the real
    tool and accepted.
    """
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(
        cmd=["python3", "tools/probe_flake.py", "--probe", PROBE,
             "--runs", str(dd.RUN_COUNT), "--rts-caps",
             str(dd.RTS_CAPABILITIES), "--result", f"{OUTSIDE}/baseline.json",
             "--artifact-root", f"{OUTSIDE}/artifacts"],
        directory=f"{CLEAN_WT}/nested")
    expect_rejected(lambda: evaluate(document),
                    "the checkout it declares keeps that tool at",
                    "a relative script naming a counterfeit nested tool")

    # The same relative spelling from the checkout ROOT is the real tool.
    fine = diagnosis_document()
    fine["baseline"]["invocation"] = invocation(
        cmd=["python3", "tools/probe_flake.py", "--probe", PROBE,
             "--runs", str(dd.RUN_COUNT), "--rts-caps",
             str(dd.RTS_CAPABILITIES), "--result", f"{OUTSIDE}/baseline.json",
             "--artifact-root", f"{OUTSIDE}/artifacts"],
        directory=CLEAN_WT)
    outcome = evaluate(fine)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"a relative script from the checkout root is fine; got "
           f"{outcome.route}")


def test_a_relabelled_check_has_changed_what_it_measures() -> None:
    """A label is the check's stated MEANING, not decoration."""
    relabelled = [("alpha", "the first check"),
                  ("beta", "an entirely different assertion"),
                  ("gamma", "the third check")]
    document = diagnosis_document()
    document["verification"]["result"] = verification_result(
        checks=relabelled,
        runs=[{cid: PASS for cid, _l in relabelled}] * dd.RUN_COUNT)
    expect_rejected(lambda: evaluate(document), "relabels",
                    "a verification that relabelled a check")

    baseline = diagnosis_document()
    baseline["baseline"]["result"] = result_document(
        checks=relabelled, runs=failing_runs(4, declared=relabelled))
    expect_rejected(lambda: evaluate(baseline), "relabels",
                    "a baseline that relabelled a check")

    expect(dd.descriptor_of(result_document()) ==
           [{"id": cid, "label": label} for cid, label in CHECKS],
           "the descriptor is compared as identifiers AND labels")


def test_a_repair_may_not_change_the_measurement_apparatus() -> None:
    """The probe is under diagnosis; the harness that measures it is not.

    `probe_flake.measure`'s timeout and starting port are module
    constants neither CLI exposes, so a repair that lengthened
    `DEFAULT_TIMEOUT` would produce a calmer verification while both
    command records still compared equal — the two batches would have
    been run by different harnesses.
    """
    for module in dd.HARNESS_MODULES:
        document = diagnosis_document(repair={
            "commit_sha": REPAIR_COMMIT, "base_sha": BASE_COMMIT,
            "changed_paths": ["tools/role_probe.py", module]})
        expect_rejected(lambda d=document: evaluate(d),
                        "measurement apparatus",
                        f"a repair that changed {module}")

    # The inventory is CLOSED and pinned exactly, not spot-checked: a
    # spot check passes while a module quietly leaves the list, and
    # leaving one out is precisely how a repair would reach the
    # apparatus. Every path owns probe selection, launch, port or
    # resource leasing, protocol reconciliation, measurement timing and
    # construction, result recording or census intake, or diagnosis
    # semantics.
    expect(dd.HARNESS_MODULES == (
        "tools/probe_flake.py",
        "tools/probe_protocol.py",
        "tools/probe_census.py",
        "tools/probe_census_contract.py",
        "tools/probe_census_records.py",
        "tools/probe_census_summary.py",
        "tools/probe_census_storage.py",
        "tools/probe_claim.py",
        "tools/probe_claim_storage.py",
        "tools/probe_claim_lease.py",
        "tools/probe_claim_orchestration.py",
        "tools/probe_resource_lock.py",
        "tools/probe_select.py",
        "tools/probe_engine.py",
        "tools/probelib.py",
        "tools/run_probes.py",
        "tools/probe_runner_registry.py",
        "tools/probe_runner_diagnostics.py",
        "tools/probe_runner_resources.py",
        "tools/probe_runner_lifecycle.py",
        "tools/probe_runner_scheduler.py",
        "tools/deflake.py",
        "tools/deflake_diagnosis.py",
    ), f"the measurement apparatus is exactly this inventory: "
       f"{dd.HARNESS_MODULES}")
    expect("tools/role_probe.py" not in dd.HARNESS_MODULES,
           "and not the probes it runs")

    # Every named path ships, so the inventory cannot drift into
    # excluding a module that no longer exists while a renamed
    # replacement goes unguarded.
    tools = Path(TOOL).parent
    for module in dd.HARNESS_MODULES:
        expect((tools.parent / module).is_file(),
               f"{module} is a real tracked module")


def test_the_defaults_no_command_line_names_are_pinned() -> None:
    """Neither CLI exposes them, so the default is the only value there is.

    A record naming another one describes a run that did not happen —
    which is what makes this stronger than comparing the three records to
    each other: setting all three to the same arbitrary value would agree
    perfectly and still be fiction.
    """
    for field, wrong in (("timeout_seconds", probe_flake.DEFAULT_TIMEOUT * 3),
                         ("start_port", probe_flake.PORT_MIN + 200)):
        for section in ("baseline", "verification"):
            absent = diagnosis_document()
            del absent[section]["invocation"][field]
            expect_rejected(lambda d=absent: evaluate(d),
                            f"records no `{field}`",
                            f"a {section} that recorded no {field}")

        # The producer spells this one `timeout`; the controlled records,
        # which this module defines, spell it `timeout_seconds`.
        producer_field = PRODUCER_FIELD.get(field, field)

        # All three altered together, which no comparison between them
        # could catch.
        document = diagnosis_document()
        document["handoff"]["invocation"][producer_field] = wrong
        for section in ("baseline", "verification"):
            document[section]["invocation"][field] = wrong
        expect_rejected(lambda d=document: evaluate(d),
                        "the only value a real measurement can have used",
                        f"every record altered to another {field}")

        handoff = handoff_document()
        del handoff["invocation"][producer_field]
        expect_rejected(lambda d=handoff: dd.require_handoff(d),
                        f"records no `{field}`",
                        f"a handoff that recorded no {producer_field}")


def test_the_conditions_a_measurement_ran_under_include_the_defaults() -> None:
    """`effective_settings` carries them, even though pinning hides it.

    `require_invocation` pins `timeout_seconds` and `start_port` to the
    harness's own values, so two well-formed records can never differ
    here and no end-to-end case can reach this comparison. It is asserted
    directly instead: the settings are what a measurement RAN UNDER, and
    a future `--timeout` flag would make the comparison load-bearing
    without anyone having to remember to add it.
    """
    settings = dd.effective_settings(invocation(), "invocation",
                                     result=result_document())
    expect(settings["timeout_seconds"] == probe_flake.DEFAULT_TIMEOUT,
           f"the timeout is a condition: {settings}")
    expect(settings["start_port"] == probe_flake.PORT_MIN,
           f"and so is the starting port: {settings}")

    altered = invocation(timeout=probe_flake.DEFAULT_TIMEOUT * 2,
                         start_port=probe_flake.PORT_MIN + 1)
    differences = dd.invocation_differences(
        invocation(), altered,
        results=(result_document(), result_document()))
    expect(any("timeout_seconds" in d for d in differences)
           and any("start_port" in d for d in differences),
           f"and both are compared when they differ: {differences}")


def test_the_baseline_replays_the_handoffs_own_conditions() -> None:
    """The chain is handoff -> baseline -> verification, not a pair.

    Comparing only the last pair let BOTH controlled batches agree on
    some arbitrary condition while the handoff sat at the defaults — and
    an agreement between two batches is not the measurement the handoff
    was taken under.

    Driven with the RUN COUNT and CAPABILITY COUNT, which are the
    conditions a command line can actually carry. `timeout_seconds` and
    `start_port` are pinned to the harness's own values by
    `require_invocation` before any comparison, so they cannot differ
    between two well-formed records at all.
    """
    document = diagnosis_document()
    for section in ("baseline", "verification"):
        tree = CLEAN_WT if section == "baseline" else REPAIR_WT
        artifacts = (f"{OUTSIDE}/artifacts" if section == "baseline"
                     else VERIFY_ARTIFACTS)
        document[section]["invocation"] = invocation(
            cmd=command(rts_caps=8, worktree=tree, artifacts=artifacts,
                        result=(f"{OUTSIDE}/baseline.json"
                                if section == "baseline"
                                else f"{OUTSIDE}/verify.json")),
            directory=tree)
        document[section]["result"] = (
            result_document(runs=failing_runs(4), rts_caps=8)
            if section == "baseline"
            else verification_result(rts_caps=8))
    expect(dd.invocation_differences(document["baseline"]["invocation"],
                                     document["verification"]["invocation"],
                                     results=(document["baseline"]["result"],
                                              document["verification"]["result"]))
           == [], "the two controlled batches agree with each other")
    expect_refused(lambda: evaluate(document),
                   "did not replay the conditions the handoff",
                   "two batches at a capability count /deflake never used")

    # The refusal names BOTH sides, so a reader can tell which value came
    # from where rather than seeing one label twice.
    try:
        evaluate(document)
    except dd.RouteRefused as error:
        message = str(error)
        expect("handoff 4" in message and "baseline 8" in message,
               f"the refusal names each side's own value: {message}")
    else:
        FAILURES.append("a baseline that did not replay was accepted")


def test_a_changed_path_is_repository_relative_and_traversal_free() -> None:
    """`tools/../src/…` begins with `tools/` and changes production code."""
    for label, path in (
            ("a traversal out of tools", "tools/../src/Engine/Core/Init.hs"),
            ("a traversal to the root", "tools/../../etc/passwd"),
            ("a self step", "tools/./role_probe.py"),
            ("a doubled separator", "tools//role_probe.py")):
        document = diagnosis_document(repair={
            "commit_sha": REPAIR_COMMIT, "base_sha": BASE_COMMIT,
            "changed_paths": [path]})
        expect_rejected(lambda d=document: evaluate(d),
                        "normalised repository-relative form",
                        f"a changed path with {label}")

    document = diagnosis_document(repair={
        "commit_sha": REPAIR_COMMIT, "base_sha": BASE_COMMIT,
        "changed_paths": [f"{CLEAN_WT}/tools/role_probe.py"]})
    expect_rejected(lambda: evaluate(document), "absolute path",
                    "a changed path given absolutely")


def test_a_generated_directory_name_names_a_real_instant_and_process() -> None:
    """`\\d{8}T\\d{6}Z` matches `99999999T999999Z`; no clock produced that."""
    for label, name in (
            ("an impossible date", f"{PROBE}-99999999T999999Z-4711-abcdef12"),
            ("an impossible month", f"{PROBE}-20261321T120000Z-4711-abcdef12"),
            ("an impossible hour", f"{PROBE}-20260821T250000Z-4711-abcdef12"),
            ("a process id of zero", f"{PROBE}-20260821T120000Z-0-abcdef12")):
        document = handoff_document()
        document["result"]["invocation_dir"] = f"{OUTSIDE}/artifacts/{name}"
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "not a directory this measurement created",
                        f"an invocation directory with {label}")


def test_a_malformed_record_is_refused_rather_than_crashing() -> None:
    """A validator that raises on its own input has refused nothing.

    `int(digits)` reads naturally and is a liveness bug on both paths
    that once used it: CPython caps integer-from-string conversion at
    4,300 digits and raises `ValueError` past it, so a version component
    or a pid of five thousand digits escaped as a traceback out of the
    very code that exists to refuse it. Both are compared without
    converting now, so an absurd digit run gets the controlled answer.
    """
    absurd = "9" * 6000
    problem = dd.interpreter_problem(f"python3.{absurd}")
    expect(problem is None,
           f"an absurdly long version is above the floor, not a crash: "
           f"{problem}")
    problem = dd.interpreter_problem(f"python3.{'0' * 5999}9")
    expect(problem is not None and "is not a Python 3 interpreter token"
           in problem,
           f"and a leading-zero one is malformed, not a crash: {problem}")

    name = f"{PROBE}-20260821T120000Z-{absurd}-abcdef12"
    expect(dd.invocation_name_problem(name, PROBE) is None,
           "an absurdly long pid is a positive one, not a crash")
    name = f"{PROBE}-20260821T120000Z-{'0' * 6000}-abcdef12"
    problem = dd.invocation_name_problem(name, PROBE)
    expect(problem is not None and "is not a process id" in problem,
           f"and an all-zero one is refused, not a crash: {problem}")

    # End to end, because the value of this is that `evaluate` answers.
    document = diagnosis_document()
    token = f"python3.{'0' * 20}1"
    for batch in ("baseline", "verification"):
        recorded = document[batch]["invocation"]
        recorded["command"] = [token] + recorded["command"][1:]
    expect_rejected(lambda d=document: evaluate(d),
                    "is not a Python 3 interpreter token",
                    "a controlled command with an absurd version token")


def test_only_ascii_digits_spell_a_generated_directory() -> None:
    """`\\d` in a `str` pattern matches every Unicode decimal digit.

    The harness writes this name from `strftime` and an f-string over
    `os.getpid()`, both of which emit ASCII and nothing else, so a name
    carrying Arabic-Indic or fullwidth digits was not written by the
    measurement it claims to describe — and `\\d+` accepted one while
    `int()` happily read it as a number.
    """
    for label, name in (
            ("an Arabic-Indic pid",
             f"{PROBE}-20260821T120000Z-\u0664\u0667-abcdef12"),
            ("a fullwidth pid",
             f"{PROBE}-20260821T120000Z-\uff14\uff17-abcdef12"),
            ("an Arabic-Indic stamp",
             f"{PROBE}-\u0662\u0660\u0662\u0666\u0660\u0668\u0662\u0661"
             f"T120000Z-4711-abcdef12")):
        expect(dd.invocation_name_problem(name, PROBE) is not None,
               f"{label} is not a generated name: {name!r}")

    expect(dd.INVOCATION_PID_RE.pattern == "[0-9]+"
           and dd.INVOCATION_STAMP_RE.pattern == "[0-9]{8}T[0-9]{6}Z",
           f"both patterns are ASCII-only: {dd.INVOCATION_PID_RE.pattern}, "
           f"{dd.INVOCATION_STAMP_RE.pattern}")


def test_the_generated_name_is_split_from_the_right() -> None:
    """The three generated fields come off the END; the probe stays whole.

    Every probe key registered today is lowercase `[a-z0-9_]` with no
    hyphen, so a left-to-right split happens to agree — but it would
    misattribute part of a hyphenated key to the stamp field the day one
    were registered, and then compare that fragment to the document's
    probe. Right-anchoring is unambiguous whatever the key contains.

    Driven through `invocation_name_problem` directly, because the
    behaviour only DIFFERS for a probe key this repository does not
    register, which no end-to-end fixture could carry.
    """
    stamp, pid, unique = "20260821T120000Z", "4711", "abcdef12"
    for key in ("role", "blood_gpu_lifecycle", "a-hyphenated-key", "a-b"):
        name = f"{key}-{stamp}-{pid}-{unique}"
        expect(dd.invocation_name_problem(name, key) is None,
               f"{name!r} is {key!r}'s own generated directory")
        # A left-to-right split would have read `a` as the probe here and
        # accepted it for a document whose probe is `a`.
        head = key.split("-", 1)[0]
        if head != key:
            expect(dd.invocation_name_problem(name, head) is not None,
                   f"{name!r} does not belong to the probe {head!r}")

    for label, name in (
            ("too few fields", f"{PROBE}-{stamp}-{pid}"),
            ("an empty probe segment", f"-{stamp}-{pid}-{unique}"),
            ("a uuid that is not hex", f"{PROBE}-{stamp}-{pid}-abcdefgh"),
            ("a uuid of the wrong length", f"{PROBE}-{stamp}-{pid}-abcdef1"),
            ("a pid that is not a number", f"{PROBE}-{stamp}-pid-{unique}"),
            ("a stamp of the wrong shape", f"{PROBE}-2026-08-21-{unique}")):
        expect(dd.invocation_name_problem(name, PROBE) is not None,
               f"{label} is not a generated name: {name!r}")


def test_both_spellings_of_a_value_taking_option_are_read() -> None:
    """`--runs 10` and `--runs=10` are one option to argparse, so to this.

    Read as a bare flag instead, `--runs=10` would leave the command
    naming no run count at all and be refused for a requiredness it
    satisfies. The VALUE has to survive the spelling too, which is why
    the accepted case is driven all the way through the binding that
    compares it to the result document.
    """
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(cmd=[
        "python3", f"{CLEAN_WT}/tools/probe_flake.py",
        f"--probe={PROBE}", f"--runs={dd.RUN_COUNT}",
        f"--rts-caps={dd.RTS_CAPABILITIES}",
        f"--result={OUTSIDE}/baseline.json",
        f"--artifact-root={OUTSIDE}/artifacts"])
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"the inline spelling is the same command; got {outcome.route}")

    # And the value really is read, rather than the option merely being
    # seen: a wrong one is bound to the result document and refused.
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(cmd=[
        "python3", f"{CLEAN_WT}/tools/probe_flake.py",
        f"--probe={PROBE}", f"--runs={dd.RUN_COUNT + 1}",
        f"--rts-caps={dd.RTS_CAPABILITIES}",
        f"--result={OUTSIDE}/baseline.json",
        f"--artifact-root={OUTSIDE}/artifacts"])
    expect_rejected(lambda d=document: evaluate(d),
                    "where its own result document reports",
                    "an inline --runs that disagrees with its own result")


def _zero_run_batch(token: str = "0", requested: int = 0) -> dict:
    """A baseline whose command and result agree on a count `measure` refuses.

    They have to agree: a `--runs 0` beside a normal ten-run document is
    already refused by the command-to-result binding, which would make
    the positivity rule look enforced when it was not.
    """
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(cmd=[
        "python3", f"{CLEAN_WT}/tools/probe_flake.py", "--probe", PROBE,
        "--runs", token, "--rts-caps", str(dd.RTS_CAPABILITIES),
        "--result", f"{OUTSIDE}/baseline.json",
        "--artifact-root", f"{OUTSIDE}/artifacts"])
    document["baseline"]["result"] = result_document(runs=[],
                                                     requested=requested)
    return document


def test_a_recorded_count_carries_the_producers_positive_constraint()\
        -> None:
    """`type=int` accepts `0` and `-3`; `measure` refuses both.

    `probe_flake.measure` raises before it resolves a probe or opens a
    port, so a recorded non-positive count describes a command that
    measured nothing. Left to the comparison downstream it would be a
    diagnosis ROUTE — "the baseline did not replay the handoff's
    conditions" — which reports a disagreement between two measurements
    where only one of them exists.
    """
    expect_rejected(lambda: evaluate(_zero_run_batch()),
                    "must be a positive count",
                    "a baseline command with --runs 0")

    # Zero is the only non-positive count reachable end to end: the
    # declared schema already floors `requested_runs` at zero, so a
    # NEGATIVE command value can never agree with a schema-valid result
    # document, and pairing it with a zero one would be refused by the
    # command-to-result binding instead. Driven through the parse
    # itself, which is where the constraint lives.
    for token in ("0", "-1", "-10"):
        expect_rejected(
            lambda t=token: dd._integer(t, "a count", positive=True),
            "must be a positive count",
            f"a parsed count of {token}")
        expect(dd._integer(token, "a count") == int(token),
               f"and {token} is still a perfectly good integer otherwise")

    for token in ("0", "-4"):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(
            cmd=[*_command_without("--rts-caps"), "--rts-caps", token])
        expect_rejected(lambda d=document: evaluate(d),
                        "must be a positive count",
                        f"a baseline command with --rts-caps {token}")

    expect(dd.POSITIVE_OPTIONS == ("--runs", "--rts-caps"),
           f"both of the harness's integer options carry it: "
           f"{dd.POSITIVE_OPTIONS}")
    expect(dd.HARNESS_LAUNCHER.positive == frozenset(dd.POSITIVE_OPTIONS),
           "and the launcher declares them")
    # `/deflake` exposes no integer option at all, so it declares none.
    expect(dd.DEFLAKE_LAUNCHER.positive == frozenset(),
           f"`/deflake` constrains no command-line integer: "
           f"{dd.DEFLAKE_LAUNCHER.positive}")


def test_an_option_may_not_be_repeated() -> None:
    """Argparse would keep the last value; evidence gets one spelling.

    `--runs 10 --runs 3` reads as three runs to the shipped tool and as
    ten to anyone reading the record left to right, so a duplicate is
    refused rather than resolved. It holds for both grammars, and for a
    flag as well as for a value-taking option.
    """
    for label, extra in (
            ("a repeated --runs", ["--runs", str(dd.RUN_COUNT)]),
            ("a repeated --probe", ["--probe", PROBE]),
            ("a repeated --result", ["--result", f"{OUTSIDE}/baseline.json"]),
            ("a repeated --rts-caps",
             ["--rts-caps", str(dd.RTS_CAPABILITIES)]),
            ("a repeated --artifact-root",
             ["--artifact-root", f"{OUTSIDE}/artifacts"]),
            # The inline spelling is the same option, not a second one.
            ("--runs twice, spelled two ways", [f"--runs={dd.RUN_COUNT}"])):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(
            cmd=command() + extra)
        expect_rejected(lambda d=document: evaluate(d), "repeats",
                        f"a baseline command with {label}")

    for label, extra in (("a repeated --json", ["--json"]),
                         ("a repeated --result",
                          ["--result", f"{OUTSIDE}/handoff.json"])):
        document = handoff_document()
        document["invocation"]["argv"] = (
            list(document["invocation"]["argv"]) + extra)
        expect_rejected(lambda d=document: dd.require_handoff(d), "repeats",
                        f"a handoff argv with {label}")


def test_a_harness_error_run_must_still_have_its_logs() -> None:
    """`stop_with_harness_error` is its only constructor and always passes one.

    `RunRecord.to_document` makes `artifact_dir` nullable because a
    PASSING run has none, so a null on the error run is a shape the
    schema permits and the harness never wrote — and it is the one run
    whose logs say why the stream broke.
    """
    result = result_document(runs=failing_runs(3), harness_error=True)
    retained = result["error_run"]["artifact_dir"]
    expect(retained, "the fixture's error run retains its directory")
    # A harness-error batch is not a usable comparison side, so this is
    # driven through the retention rule itself rather than through the
    # gate, which would refuse the fixture for the batch's status first
    # and hide whether the null case is checked at all.
    dd.require_result(copy.deepcopy(result), "a harness-error result")

    stripped = copy.deepcopy(result)
    stripped["error_run"]["artifact_dir"] = None
    stripped["retained_artifacts"] = [entry for entry
                                      in stripped["retained_artifacts"]
                                      if entry != retained]
    expect_rejected(lambda d=stripped: dd.require_result(d, "a result"),
                    "a failure whose logs are gone",
                    "a harness-error run that kept no artifacts")


def test_a_measurements_timestamp_is_an_instant() -> None:
    """Delegated to `probe_census.parse_timestamp`, the shipped reader."""
    for label, stamp in (("an impossible date", "2026-99-99T99:99:99Z"),
                         ("no timezone marker", "2026-08-21T12:00:00"),
                         ("a date alone", "2026-08-21"),
                         ("nothing at all", None),
                         ("a number", 20260821)):
        document = handoff_document()
        document["result"]["timestamp_utc"] = stamp
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "timestamp", f"a measurement stamped with {label}")

    for section in ("baseline", "verification"):
        document = diagnosis_document()
        document[section]["result"]["timestamp_utc"] = "2026-99-99T99:99:99Z"
        expect_rejected(lambda d=document: evaluate(d), "timestamp",
                        f"a {section} stamped with an impossible date")


def test_a_malformed_list_is_a_refusal_not_a_traceback() -> None:
    """`list(42)` raises `TypeError`; a document must never do that."""
    for field in ("expected_checks", "targets"):
        for value in (42, "beta", {"beta": True}, [42], [""], [None]):
            document = handoff_document()
            document[field] = value
            expect_rejected(lambda d=document: dd.require_handoff(d),
                            "must be a list of identifiers",
                            f"a {field} of {value!r}")


def test_a_fabricated_argv_is_not_a_harness_invocation() -> None:
    for label, fragment, cmd in (
            ("another script", "the programs that produce",
             ["python3", f"{CLEAN_WT}/tools/run_probes.py",
              "--probe", PROBE, "--runs", "10"]),
            ("an extra positional", "positional token",
             ["python3", f"{CLEAN_WT}/tools/probe_flake.py", "extra",
              "--probe", PROBE, "--runs", "10",
              "--result", f"{OUTSIDE}/b.json"]),
            ("no script at all", "is not a Python 3 interpreter token",
             ["--probe", PROBE, "--runs", "10"]),
            # The right SHAPE, running something that measures nothing.
            ("a program that is not an interpreter",
             "runs the interpreter by path",
             ["/bin/echo", f"{CLEAN_WT}/tools/probe_flake.py",
              "--probe", PROBE, "--runs", "10"]),
            ("a counterfeit interpreter", "runs the interpreter by path",
             ["/tmp/counterfeit/python3", f"{CLEAN_WT}/tools/probe_flake.py",
              "--probe", PROBE, "--runs", "10",
              "--result", f"{OUTSIDE}/b.json"]),
            ("a counterfeit script", "the checkout it declares keeps that tool at",
             ["python3", "/tmp/counterfeit/probe_flake.py",
              "--probe", PROBE, "--runs", "10",
              "--result", f"{OUTSIDE}/b.json"]),
            ("a shell", "is not a Python 3 interpreter token",
             ["sh", f"{CLEAN_WT}/tools/probe_flake.py",
              "--probe", PROBE, "--runs", "10"]),
            # Order is part of the grammar: Python rejects an option it
            # does not know BEFORE it runs the script.
            ("an option before the script", "before the script it ran",
             ["python3", "--probe", PROBE,
              f"{CLEAN_WT}/tools/probe_flake.py", "--runs", "10",
              "--result", f"{OUTSIDE}/b.json"]),
    ):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(cmd=cmd)
        expect_rejected(lambda d=document: evaluate(d), fragment,
                        f"a command with {label}")


def test_an_absent_option_compares_as_its_effective_default() -> None:
    """"The caller declined to override a default" is not a difference."""
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(cmd=[
        "python3", "/tmp/deflake-clean-role/tools/probe_flake.py",
        "--probe", PROBE, "--runs", str(dd.RUN_COUNT),
        "--result", f"{OUTSIDE}/baseline.json",
        "--artifact-root", f"{OUTSIDE}/artifacts"])
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           "an omitted --rts-caps equals an explicit one at the default")


def _shipped_options(script: str) -> set:
    done = subprocess.run(
        [sys.executable, str(Path(TOOL).parent / script), "--help"],
        capture_output=True, text=True, timeout=120)
    expect(done.returncode == 0, f"{script} --help exits 0: {done.stderr}")
    real = set(re.findall(r"(?<![\w-])(--[a-z][a-z-]*)", done.stdout))
    real.discard("--help")
    return real


def test_the_option_tables_match_the_shipped_tools() -> None:
    """Drift guard: the real `--help` is the authority on each surface.

    Both tools build their parsers inside `main`, so the tables here are
    hard-coded — and this reads each shipped CLI's own help output, so
    adding, removing or renaming an option fails here instead of
    silently widening what this module will accept.
    """
    for launcher in (dd.HARNESS_LAUNCHER, dd.DEFLAKE_LAUNCHER):
        real = _shipped_options(launcher.script)
        expect(real == set(launcher.options),
               f"{launcher.script}'s classified options "
               f"{sorted(launcher.options)} are exactly the shipped ones "
               f"{sorted(real)}")
        expect(set(launcher.required) <= set(launcher.options),
               f"and {launcher.script}'s required options are ones it has")
        expect(set(launcher.destinations) <= set(launcher.options),
               f"and so are its destinations")
        expect(launcher.values.isdisjoint(launcher.flags),
               f"and no {launcher.script} option both takes a value and does not")

    real = _shipped_options(dd.HARNESS_LAUNCHER.script)
    expect(real == set(dd.HARNESS_OPTIONS),
           f"the classified options {sorted(dd.HARNESS_OPTIONS)} are exactly "
           f"the shipped ones {sorted(real)}")
    expect(dd.DEFLAKE_LAUNCHER.fixed["runs"] == deflake.CENSUS_RUN_COUNT
           and dd.DEFLAKE_LAUNCHER.fixed["rts_caps"] == deflake.RTS_CAPABILITIES,
           "and /deflake's fixed contract is its own module's constants")
    expect(set(dd.CONDITION_OPTIONS).isdisjoint(dd.DESTINATION_OPTIONS),
           "and no option is both a condition and a destination")
    expect(set(dd.HARNESS_OPTIONS) ==
           set(dd.CONDITION_OPTIONS) | set(dd.DESTINATION_OPTIONS),
           "every classified option is a condition or a destination")
    expect(set(dd.REQUIRED_OPTIONS) <= set(dd.HARNESS_OPTIONS),
           "and every required option is one the harness has")


# ==========================================================================
# Stable check identity
# ==========================================================================
def test_a_renamed_identifier_is_refused() -> None:
    document = diagnosis_document()
    renamed = [("alpha", "the first check"), ("beta_two", "renamed"),
               ("gamma", "the third check")]
    document["verification"]["result"] = verification_result(checks=renamed,
        runs=[{cid: PASS for cid, _l in renamed}] * dd.RUN_COUNT)
    expect_rejected(lambda: evaluate(document),
                    "separately approved protocol change",
                    "a verification that renamed a check")


def test_a_removed_identifier_is_refused() -> None:
    document = diagnosis_document()
    fewer = [("alpha", "the first check"), ("beta", "the second check")]
    document["verification"]["result"] = verification_result(checks=fewer,
        runs=[{cid: PASS for cid, _l in fewer}] * dd.RUN_COUNT)
    expect_rejected(lambda: evaluate(document),
                    "separately approved protocol change",
                    "a verification that dropped a check")


def test_a_reordered_descriptor_is_refused() -> None:
    document = diagnosis_document()
    swapped = [CHECKS[1], CHECKS[0], CHECKS[2]]
    document["verification"]["result"] = verification_result(checks=swapped,
        runs=[{cid: PASS for cid, _l in swapped}] * dd.RUN_COUNT)
    expect_rejected(lambda: evaluate(document),
                    "separately approved protocol change",
                    "a verification that reordered the descriptor")


def test_an_identifier_carrying_a_runtime_value_is_malformed() -> None:
    """`probe-result/v1` identifiers are static; a digit is how a value gets in.

    The descriptor and every run agree on the name, so the identifier's
    SHAPE is the only rule this fixture breaks — a document that merely
    disagreed with itself would be caught by the undeclared-identifier
    rule instead and prove nothing about this one.
    """
    valued = [("alpha", "the first check"), ("beta_two", "the second check"),
              ("gamma", "the third check")]
    document = handoff_document(result=result_document(
        checks=valued,
        runs=failing_runs(3, cid="beta_two", declared=valued)))
    accepted = dd.require_handoff(copy.deepcopy(document))
    expect(accepted.probe == PROBE,
           "a spelled-out number is a legitimate identifier")
    for entry in document["result"]["checks"]:
        if entry["id"] == "beta_two":
            entry["id"] = "beta_2"
    for run in document["result"]["runs"]:
        run["checks"]["beta_2"] = run["checks"].pop("beta_two")
    document["result"]["check_counts"]["beta_2"] = (
        document["result"]["check_counts"].pop("beta_two"))
    document["targets"] = [("beta_2" if cid == "beta_two" else cid)
                           for cid in document["targets"]]
    expect_rejected(lambda: dd.require_handoff(document),
                    "identifiers are static",
                    "an identifier carrying a measured value")


def test_a_run_reporting_an_undeclared_identifier_is_malformed() -> None:
    """Delegated: `probe_census.validate_result` owns the tally rules."""
    document = handoff_document()
    document["result"]["runs"][0]["checks"]["delta"] = PASS
    expect_rejected(lambda: dd.require_handoff(document),
                    "internally inconsistent",
                    "a run reporting an undeclared check")


def test_a_run_that_simply_omits_a_declared_identifier_is_malformed() -> None:
    """A key the harness always writes, absent with a tally that agrees.

    Kept as a rule of this module's own because it is exactly the shape
    the canonical validator cannot see: `check_counts` is derived from
    `runs`, so a document whose tally was lowered to match the omission
    is internally consistent and still was not written by the harness.
    """
    document = handoff_document()
    dropped = document["result"]["runs"][0]["checks"].pop("gamma")
    document["result"]["check_counts"]["gamma"][dropped] -= 1
    expect_rejected(lambda: dd.require_handoff(document),
                    "was not written by the harness",
                    "a run whose check map lost a key")


# ==========================================================================
# MISSING
# ==========================================================================
def test_a_target_has_zero_missing_however_many_runs_may_fail() -> None:
    """The approved rule, isolated, and independent of X.

    Asserted against `missing_problems` directly, because inside
    `evaluate` the failure COUNT also refuses a batch with three aborted
    runs — driving it end to end would prove the count rule and say
    nothing about this one.
    """
    document = result_document(commit=REPAIR_COMMIT,
                               runs=failing_runs(3, cid="beta"))
    problems = dd.missing_problems(document, targets={"gamma"},
                                   what="the verification batch")
    expect(any("zero MISSING" in problem for problem in problems),
           f"gamma is MISSING in the aborted runs, so it is refused: "
           f"{problems}")

    expect(dd.missing_problems(document, targets={"beta"},
                               what="the verification batch") == [],
           "beta is emitted in every run, so it is not")

    spotless = result_document(commit=REPAIR_COMMIT)
    expect(dd.missing_problems(spotless, targets={"gamma"}, what="x") == [],
           "and a batch that aborted nowhere satisfies it outright")


def test_a_target_that_stops_being_emitted_is_refused_end_to_end() -> None:
    """One accepted failing run may lose it; more than X may not."""
    handoff = handoff_document(acceptable=1)
    document = diagnosis_document(handoff=handoff)
    document["verification"]["result"] = verification_result(runs=failing_runs(2))
    expect_refused(lambda: evaluate(document), "partial-improvement",
                   "a target lost in more runs than X allows")


def test_a_passing_run_may_not_omit_a_check() -> None:
    document = diagnosis_document()
    runs = [{"alpha": PASS, "beta": PASS, "gamma": MISSING}]
    runs += [{cid: PASS for cid, _l in CHECKS}] * (dd.RUN_COUNT - 1)
    document["verification"]["result"] = verification_result(runs=runs)
    expect_refused(lambda: evaluate(document),
                   "passed while omitting",
                   "a passing run that omitted a check")


def test_an_accepted_failing_run_may_abort_after_the_targets() -> None:
    """The suffix allowance is for the checks that are NOT targets.

    A target has zero MISSING across all ten runs, so an accepted failing
    run may abort — but only AFTER every target. Here the handoff fails
    at `alpha` without aborting, so `alpha` alone is the target, and the
    verification's one accepted failing run aborts at `beta`, losing only
    non-targets.
    """
    handoff = handoff_document(acceptable=1, result=result_document(
        runs=failing_runs(3, cid="alpha", abort=False)))
    expect(list(handoff["targets"]) == ["alpha"],
           f"a non-aborting failure implicates only itself; got "
           f"{handoff['targets']}")
    document = diagnosis_document(handoff=handoff)
    document["baseline"]["result"] = result_document(
        runs=failing_runs(4, cid="alpha", abort=False))
    document["verification"]["result"] = verification_result(
        runs=failing_runs(1, cid="beta"))
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"an abort after every target is accepted; got {outcome.route}")

    # A run that aborted BEFORE the target is refused, however few such
    # runs the batch has: a run that never reached the target did not
    # demonstrate the target was fixed.
    losing = diagnosis_document(handoff=handoff)
    losing["baseline"]["result"] = result_document(
        runs=failing_runs(4, cid="alpha", abort=False))
    runs = [{"__timeout__": True, "alpha": MISSING, "beta": MISSING,
             "gamma": MISSING}]
    runs += [{cid: PASS for cid, _l in CHECKS}] * (dd.RUN_COUNT - 1)
    losing["verification"]["result"] = verification_result(runs=runs)
    expect_refused(lambda: evaluate(losing),
                   "a target has zero MISSING across all",
                   "an accepted failing run that lost a target")


def test_a_non_contiguous_gap_is_malformed_rather_than_an_abort() -> None:
    document = diagnosis_document()
    document["handoff"]["acceptable_failures"] = 1
    runs = [{"alpha": MISSING, "beta": FAIL, "gamma": MISSING}]
    runs += [{cid: PASS for cid, _l in CHECKS}] * (dd.RUN_COUNT - 1)
    document["verification"]["result"] = verification_result(runs=runs)
    expect_refused(lambda: evaluate(document), "contiguous suffix",
                   "a run with a hole in the middle of its results")


def test_an_identifier_that_vanishes_from_the_batch_is_refused() -> None:
    document = diagnosis_document()
    document["handoff"]["acceptable_failures"] = 3
    runs = [{"alpha": PASS, "beta": FAIL, "gamma": MISSING}] * dd.RUN_COUNT
    document["verification"]["result"] = verification_result(runs=runs)
    expect_refused(lambda: evaluate(document),
                   "never emitted gamma",
                   "a check that was never emitted in the whole batch")


def test_a_missing_violation_is_the_partial_improvement_route() -> None:
    """At or below X but with a MISSING result is #1439, not a PR."""
    document = diagnosis_document(route=dd.ROUTE_PARTIAL_IMPROVEMENT)
    runs = [{"alpha": PASS, "beta": PASS, "gamma": MISSING}]
    runs += [{cid: PASS for cid, _l in CHECKS}] * (dd.RUN_COUNT - 1)
    document["verification"]["result"] = verification_result(runs=runs)
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_PARTIAL_IMPROVEMENT,
           f"a MISSING violation routes to #1439; got {outcome.route}")
    expect(outcome.owner_issue == 1439, "and names its owner")


# ==========================================================================
# Every route
# ==========================================================================
def test_the_repair_route_is_the_only_one_that_opens_a_pull_request() -> None:
    outcome = evaluate(diagnosis_document())
    expect(outcome.route == dd.ROUTE_REPAIR, "a verified repair is a repair")
    expect(outcome.opens_pull_request, "and it opens the one pull request")
    expect(outcome.owner_issue is None, "with no downstream owner")


def test_cannot_reproduce_hands_off_to_1439() -> None:
    document = diagnosis_document(route=dd.ROUTE_CANNOT_REPRODUCE)
    document["baseline"]["result"] = result_document()
    document.pop("verification", None)
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_CANNOT_REPRODUCE, "the route holds")
    expect(outcome.owner_issue == 1439, "and hands off to #1439")
    expect(not outcome.opens_pull_request, "opening no pull request")


def test_every_non_repair_route_preserves_its_evidence() -> None:
    """#1439 and #1438 receive the evidence, so a route without it is refused."""
    for route in (dd.ROUTE_CANNOT_REPRODUCE, dd.ROUTE_PRODUCTION_DEFECT,
                  dd.ROUTE_NO_CONFIDENT_FIX, dd.ROUTE_PARTIAL_IMPROVEMENT):
        document = diagnosis_document(route=route)
        if route == dd.ROUTE_CANNOT_REPRODUCE:
            document["baseline"]["result"] = result_document()
            document.pop("verification", None)
        if route == dd.ROUTE_PARTIAL_IMPROVEMENT:
            document["verification"]["result"] = verification_result(runs=failing_runs(2))
        del document["diagnosis"]
        expect_rejected(lambda d=document: evaluate(d),
                        "states no diagnosis",
                        f"a {route} route with no diagnosis")


def test_cannot_reproduce_is_refused_when_it_did_reproduce() -> None:
    document = diagnosis_document(route=dd.ROUTE_CANNOT_REPRODUCE)
    expect_refused(lambda: evaluate(document), "DID reproduce",
                   "cannot-reproduce declared over a reproducing baseline")


def test_the_production_defect_route_hands_off_to_1438() -> None:
    document = diagnosis_document(route=dd.ROUTE_PRODUCTION_DEFECT,
                                  diagnosis={
                                      "summary": "the engine really does "
                                                 "drop the order",
                                      "evidence": ["the engine log shows the "
                                                   "order accepted and never "
                                                   "executed"]})
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_PRODUCTION_DEFECT, "the route holds")
    expect(outcome.owner_issue == 1438, "and hands off to #1438")
    expect(not outcome.opens_pull_request, "opening no pull request")


def test_a_non_repair_route_may_not_carry_a_verification_batch() -> None:
    """A verification means a repair was attempted, so the route is wrong."""
    document = diagnosis_document(route=dd.ROUTE_PRODUCTION_DEFECT,
                                  diagnosis={"summary": "the product is wrong",
                                             "evidence": ["engine log"]})
    document["verification"] = {
        "worktree": REPAIR_WT, "source_clean": True,
        "result": verification_result(),
        "invocation": invocation(directory=REPAIR_WT),
        "configuration": manifest(),
    }
    expect_refused(lambda: evaluate(document), "runs no verification batch",
                   "a production-defect route carrying a verification")


def test_the_no_confident_fix_route_hands_off_to_1439() -> None:
    document = diagnosis_document(route=dd.ROUTE_NO_CONFIDENT_FIX,
                                  diagnosis={
                                      "summary": "three failures, three "
                                                 "unrelated candidates",
                                      "evidence": ["no single change moved "
                                                   "all three"]})
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_NO_CONFIDENT_FIX, "the route holds")
    expect(outcome.owner_issue == 1439, "and hands off to #1439")


def test_partial_improvement_hands_off_to_1439() -> None:
    document = diagnosis_document(route=dd.ROUTE_PARTIAL_IMPROVEMENT)
    document["verification"]["result"] = verification_result(runs=failing_runs(2))
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_PARTIAL_IMPROVEMENT, "the route holds")
    expect(outcome.owner_issue == 1439, "and hands off to #1439")
    expect(outcome.baseline_failures == 4 and outcome.verification_failures == 2,
           "and reports both counts")


def test_an_invalid_verification_batch_is_1439_not_a_rejection() -> None:
    """The issue lists "becomes invalid" beside "remains above X".

    Both are #1439 outcomes with the evidence preserved, so a harness
    error in the verification batch must reach `partial-improvement` —
    reporting it as a rejected handoff would lose the retained artifacts
    and describe an invocation that never got past the gate.
    """
    for label, result, runs in (
            ("a harness error", verification_result(
                runs=failing_runs(1), harness_error=True), None),
            ("a short batch", verification_result(
                runs=failing_runs(1)[:5], requested=5, command_runs=5), 5),
            ("a contended machine", None, None),
    ):
        document = diagnosis_document(route=dd.ROUTE_PARTIAL_IMPROVEMENT)
        if result is None:
            document["verification"]["result"]["peak_concurrency"] = 2
        else:
            document["verification"]["result"] = result
        if runs is not None:
            # The command is bound to its own result, so a batch of
            # another size has to have ASKED for that size.
            document["verification"]["invocation"] = invocation(
                cmd=command(runs=runs, result=f"{OUTSIDE}/verify.json",
                            artifacts=VERIFY_ARTIFACTS, worktree=REPAIR_WT),
                directory=REPAIR_WT, ports=[9201])
        outcome = evaluate(document)
        expect(outcome.route == dd.ROUTE_PARTIAL_IMPROVEMENT,
               f"{label} routes to #1439; got {outcome.route}")
        expect(outcome.owner_issue == 1439, f"{label} names its owner")

    repair = diagnosis_document()
    repair["verification"]["result"]["peak_concurrency"] = 2
    expect_refused(lambda: evaluate(repair), "partial-improvement",
                   "a repair declared over a contended verification")


def test_an_invalid_baseline_is_cannot_reproduce_not_a_rejection() -> None:
    document = diagnosis_document(route=dd.ROUTE_CANNOT_REPRODUCE)
    document["baseline"]["result"] = result_document(
        runs=failing_runs(4), harness_error=True)
    document.pop("verification", None)
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_CANNOT_REPRODUCE,
           f"an aborted baseline established nothing; got {outcome.route}")
    expect(outcome.owner_issue == 1439, "and hands off to #1439")

    repair = diagnosis_document()
    repair["baseline"]["result"] = result_document(
        runs=failing_runs(4), harness_error=True)
    expect_refused(lambda: evaluate(repair), "cannot-reproduce",
                   "a repair declared over an aborted baseline")


def test_an_over_tolerance_baseline_that_is_still_invalid_is_refused() -> None:
    """An invalid baseline is never a repair, whatever its failure count.

    A harness error also leaves the batch incomparable on run count, so
    the tolerance rule would refuse it anyway. This one is over
    tolerance, hits the target, and is unusable ONLY because the machine
    was contended — which makes the "not a usable measurement" rule the
    single thing standing between it and a pull request.
    """
    document = diagnosis_document()
    document["baseline"]["result"]["peak_concurrency"] = 2
    expect_refused(lambda: evaluate(document),
                   "established nothing to repair from",
                   "a repair declared over a contended baseline")


def test_a_command_missing_a_required_option_never_ran() -> None:
    for option in dd.REQUIRED_OPTIONS:
        cmd = [token for token in command()]
        index = cmd.index(option)
        del cmd[index:index + 2]
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(cmd=cmd)
        expect_rejected(lambda d=document: evaluate(d),
                        f"names no {option}",
                        f"a command with no {option}")


def test_a_section_with_no_worktree_at_all_is_refused() -> None:
    for value in (None, "", 42):
        document = diagnosis_document()
        if value is None:
            del document["verification"]["worktree"]
        else:
            document["verification"]["worktree"] = value
        expect_rejected(lambda d=document: evaluate(d), "names no worktree",
                        f"a section whose worktree is {value!r}")


def test_an_artifact_layout_inside_a_worktree_is_refused() -> None:
    """Raw artifacts never land in a repository worktree.

    The whole layout moves together, because `require_topology` pins the
    invocation directory and every run directory to the artifact root —
    so a batch cannot put one of them in a worktree while leaving the
    others outside, and the honest case is a root that is itself inside
    one.
    """
    for label, root in (("the repair worktree", f"{REPAIR_WT}/artifacts"),
                        ("the clean worktree", f"{CLEAN_WT}/artifacts")):
        document = diagnosis_document(handoff=handoff_document(acceptable=1))
        document["verification"]["result"] = verification_result(
            runs=failing_runs(1, abort=False), artifact_root=root)
        document["verification"]["invocation"] = invocation(
            cmd=command(result=f"{OUTSIDE}/verify.json", artifacts=root,
                        worktree=REPAIR_WT),
            directory=REPAIR_WT, ports=[9201])
        expect_rejected(lambda d=document: evaluate(d, worktrees=()),
                        "inside the working tree",
                        f"a verification whose artifacts live in {label}")


def test_the_handoffs_own_evidence_may_not_live_in_a_worktree() -> None:
    """The handoff is held to the batches' containment rule, not exempt.

    Checked with NO registered worktrees, because the comparison
    worktrees the diagnosis DECLARES are collected before the handoff is
    admitted — which is what still holds after they are removed.
    """
    # Built THROUGH the producer from that result, so `artifacts` and
    # `invocation.ports` follow it: swapping the result alone would break
    # the envelope's equality rules instead of the containment rule.
    document = diagnosis_document(handoff=handoff_document(
        result=result_document(runs=failing_runs(3),
                               artifact_root=f"{CLEAN_WT}/artifacts")))
    expect_rejected(lambda: evaluate(document, worktrees=()),
                    "inside the working tree",
                    "a handoff whose artifact tree is in a comparison worktree")

    moved = diagnosis_document()
    moved["handoff"]["invocation"]["argv"] = [
        f"{PRIMARY_WT}/tools/deflake.py", "--json",
        "--result", f"{REPAIR_WT}/handoff.json"]
    expect_rejected(lambda: evaluate(moved, worktrees=()),
                    "inside the working tree",
                    "a handoff result document written into a worktree")

    # A handoff can no longer name an EXTRA kept path at all: the approved
    # envelope rule makes `artifacts` equal `result.retained_artifacts`
    # exactly, and that list is `_require_retention`'s derived view of the
    # runs' own directories. So the stricter rule answers first, and a
    # path inside a worktree can only arrive through the artifact ROOT —
    # which is the first case above.
    extra = diagnosis_document()
    extra["handoff"]["artifacts"] = [f"{REPAIR_WT}/kept"]
    expect_rejected(lambda: evaluate(extra, worktrees=()),
                    "cannot disagree",
                    "a handoff naming a kept path its result never retained")


def test_a_default_artifact_root_inside_a_worktree_is_still_refused() -> None:
    """`--artifact-root` is optional, and that is where this rule earns its keep.

    With the option present, the agreement rule ties the reported root to
    a destination that is already containment-checked. Omitted — which is
    legitimate, since `probe_flake.default_artifact_root` supplies a
    temporary directory — nothing else constrains the root the document
    reports, so the sweep over the paths a result NAMES is the only thing
    standing between a worktree-resident layout and `repair-pr`.
    """
    document = diagnosis_document(handoff=handoff_document(acceptable=1))
    document["verification"]["result"] = verification_result(
        runs=failing_runs(1, abort=False),
        artifact_root=f"{REPAIR_WT}/artifacts")
    document["verification"]["invocation"] = invocation(
        cmd=["python3", f"{REPAIR_WT}/tools/probe_flake.py",
             "--probe", PROBE, "--runs", str(dd.RUN_COUNT),
             "--rts-caps", str(dd.RTS_CAPABILITIES),
             "--result", f"{OUTSIDE}/verify.json"],
        directory=REPAIR_WT, ports=[9201])
    expect_rejected(lambda: evaluate(document, worktrees=()),
                    "inside the working tree",
                    "a default artifact root inside the repair worktree")

    outside = diagnosis_document(handoff=handoff_document(acceptable=1))
    outside["verification"]["result"] = verification_result(
        runs=failing_runs(1, abort=False),
        artifact_root=f"{OUTSIDE}/defaulted")
    outside["verification"]["invocation"] = invocation(
        cmd=["python3", f"{REPAIR_WT}/tools/probe_flake.py",
             "--probe", PROBE, "--runs", str(dd.RUN_COUNT),
             "--rts-caps", str(dd.RTS_CAPABILITIES),
             "--result", f"{OUTSIDE}/verify.json"],
        directory=REPAIR_WT, ports=[9201])
    outcome = evaluate(outside, worktrees=())
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"and omitting the option is otherwise fine; got {outcome.route}")


def test_the_result_paths_a_document_names_are_all_of_them() -> None:
    """What the containment sweep covers, pinned.

    With `--artifact-root` supplied, topology derives every other path
    from a root the agreement rule has already tied to a checked
    destination — so this list is what the sweep still owns when the
    option is omitted, and it must not silently stop covering any of it.
    """
    document = result_document(runs=failing_runs(2), harness_error=True)
    labels = {label for label, _path in dd.result_paths(document)}
    expect("artifact_root" in labels and "invocation_dir" in labels,
           f"the root and the invocation directory are swept: {labels}")
    expect(any(label.startswith("runs[") for label in labels),
           f"and every run's directory: {labels}")
    expect("error_run.artifact_dir" in labels,
           f"and the run that broke the stream: {labels}")
    expect(any(label.startswith("retained_artifacts[") for label in labels),
           f"and every retained entry: {labels}")

    inside = [path for _label, path in dd.result_paths(document)
              if dd.inside_any_worktree(path, [REPAIR_WT]) is not None]
    expect(inside == [], "an external layout is outside every worktree")
    moved = result_document(runs=failing_runs(2), harness_error=True,
                            artifact_root=f"{REPAIR_WT}/artifacts")
    inside = [path for _label, path in dd.result_paths(moved)
              if dd.inside_any_worktree(path, [REPAIR_WT]) is not None]
    expect(len(inside) == len(dd.result_paths(moved)),
           f"and a layout rooted in one is entirely inside it: {inside}")


def test_the_command_and_its_result_must_describe_one_measurement() -> None:
    document = diagnosis_document()
    document["verification"]["result"] = verification_result(
        artifact_root=f"{OUTSIDE}/somewhere-else")
    expect_rejected(lambda: evaluate(document),
                    "have to describe one measurement",
                    "a result reporting a root its command never named")


def test_every_route_hands_on_every_batchs_retained_artifacts() -> None:
    """#1439 and #1438 are handed the evidence, so it has to be named.

    The batch that went wrong is usually the VERIFICATION, whose logs an
    outcome built from the handoff alone would never mention at all.
    """
    document = diagnosis_document(route=dd.ROUTE_PARTIAL_IMPROVEMENT,
                                  handoff=handoff_document(acceptable=1))
    document["verification"]["result"] = verification_result(
        runs=failing_runs(2), harness_error=True)
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_PARTIAL_IMPROVEMENT,
           f"an aborted verification is #1439; got {outcome.route}")
    for label, expected in (
            ("handoff", document["handoff"]["result"]["retained_artifacts"]),
            ("baseline", document["baseline"]["result"]["retained_artifacts"]),
            ("verification",
             document["verification"]["result"]["retained_artifacts"])):
        missing = [path for path in expected
                   if path not in outcome.artifacts]
        expect(not missing,
               f"the outcome names the {label} batch's retained artifacts; "
               f"missing {missing} from {outcome.artifacts}")
    expect(len(outcome.artifacts) == len(set(outcome.artifacts)),
           f"and names each one once: {outcome.artifacts}")
    expect(outcome.to_document()["retained_artifacts"] == outcome.artifacts,
           "and the emitted document carries the same list")


DIGEST_A = "a" * 64


DIGEST_B = "b" * 64


LOCAL_YAML = "config/video.local.yaml"


def real_cli_argv(*extra: str) -> list:
    """The argv a REAL `python3 tools/deflake.py ...` process observes.

    Captured from an actual subprocess rather than written out, because
    writing it out is the mistake this exists to stop: both this suite
    and `tools/test_deflake.py` spelled it `["python3", "tools/deflake.py",
    ...]`, and `deflake.main` passes `list(sys.argv)`, whose [0] is the
    SCRIPT. An interpreter-first fixture therefore certified an entry
    gate that no handoff from the real CLI could pass.

    The subprocess runs the launcher with `-c`-free argv and prints what
    `sys.argv` actually is, so the FORM comes from Python rather than
    from this file's belief about it.
    """
    captured = subprocess.run(
        [sys.executable, "-c",
         "import json, sys; print(json.dumps(sys.argv))",
         *extra],
        capture_output=True, text=True, check=True)
    observed = json.loads(captured.stdout)
    # `-c` occupies argv[0] the way a script path would; the invariant
    # under test is that argv[0] is NOT the interpreter and the options
    # follow it directly.
    expect(observed[0] != sys.executable and observed[0] != "python3",
           f"argv[0] is never the interpreter; got {observed[0]!r}")
    expect(observed[1:] == list(extra),
           f"and every remaining token is the script's own: {observed}")
    # The FORM is what the subprocess established; the PATH is the
    # checkout this fixture declares, since that is what the gate
    # resolves the script against.
    return [f"{PRIMARY_WT}/tools/deflake.py", *extra]


def test_a_handoff_from_the_real_cli_path_is_admitted() -> None:
    """End to end across the seam that broke: producer -> entry gate.

    `deflake.measure_next_probe` is the real producer, driven here the
    way `tools/test_deflake.py` drives it — every collaborator injected,
    no probe executed — with the argv form taken from a real subprocess.
    The handoff it WRITES is then handed to the entry gate unmodified.

    This is the case a hand-assembled fixture cannot be: the envelope's
    keys, its argv form, its ports, its targets and its artifacts all
    come from the shipped producer, so a gate that disagrees with #1659
    about any of them fails here rather than in production.
    """
    argv = real_cli_argv("--json", "--result", f"{OUTSIDE}/handoff.json")
    result = result_document(runs=failing_runs(3))
    document = deflake.build_handoff(
        result=result,
        acceptable_failures=1,
        argv=argv,
        cwd=PRIMARY_WT,
        configuration=deflake.configuration_manifest(
            primary_config_root()),
        artifacts=list(result["retained_artifacts"]))

    expect(document["invocation"]["argv"][0].endswith("deflake.py"),
           f"the producer records the script at argv[0]; got "
           f"{document['invocation']['argv'][0]!r}")
    expect("python3" not in document["invocation"]["argv"],
           f"and no interpreter token at all: {document['invocation']['argv']}")

    accepted = dd.require_handoff(copy.deepcopy(document), primary=PRIMARY_WT)
    expect(accepted.probe == PROBE,
           "the entry gate admits what the real CLI path produces")
    expect(accepted.targets == tuple(document["targets"]),
           f"with the producer's own targets: {accepted.targets}")
    expect(list(accepted.invocation["ports"])
           == [run["port"] for run in result["runs"]],
           "and the producer's own ports")

    # And it survives the whole diagnosis, not merely the gate.
    diagnosis = diagnosis_document(handoff=document)
    expect(evaluate(diagnosis).route == dd.ROUTE_REPAIR,
           "and a diagnosis built on it reaches its route")


def test_the_entry_gate_reads_the_producers_own_spelling() -> None:
    """`argv`/`cwd`/`timeout`, because that is what #1659 writes.

    This is the regression that mattered: requiring `command`/`directory`
    — the names the CONTROLLED batches use, which this module defines
    itself — rejected every real handoff for "no `directory`", so the
    workflow could not consume its own prerequisite. The messages are
    pinned because a producer document diagnosed against the internal
    spelling is exactly the confusion that hid the bug.
    """
    for field, fragment in (("cwd", "records the directory it ran in"),
                            ("argv", "records the command it ran")):
        document = handoff_document()
        del document["invocation"][field]
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        fragment,
                        f"a handoff invocation with no `{field}`")

    # And the internal spelling is NOT a second accepted form: a document
    # naming `command`/`directory` is not one the producer wrote.
    document = handoff_document()
    document["invocation"] = invocation()
    expect_rejected(lambda: dd.require_handoff(document),
                    "records the directory it ran in",
                    "a handoff invocation in the controlled batches' shape")

    # The producer's own output goes straight through.
    built = deflake.build_handoff(
        result=result_document(runs=failing_runs(3)),
        acceptable_failures=0, argv=deflake_argv(), cwd=PRIMARY_WT,
        configuration=[],
        artifacts=list(result_document(
            runs=failing_runs(3))["retained_artifacts"]))
    accepted = dd.require_handoff(built, primary=PRIMARY_WT)
    expect(accepted.probe == PROBE,
           "a handoff `deflake.build_handoff` actually produced is admitted")
    expect(accepted.invocation["directory"] == PRIMARY_WT,
           "and its `cwd` reaches the internal record as `directory`")
    expect(accepted.invocation["timeout_seconds"] == deflake.TIMEOUT,
           "and its `timeout` as `timeout_seconds`")


def test_the_envelope_redundancies_are_enforced() -> None:
    """Values the producer DERIVED from the result cannot disagree with it.

    `deflake.build_handoff` reads `artifacts` from
    `measurement.retained_artifacts()` and `ports` from the result's own
    runs, so each is one list recorded twice. `probe_census.validate_result`
    checks neither.
    """
    document = handoff_document()
    document["artifacts"] = list(document["artifacts"]) + ["/tmp/extra"]
    expect_rejected(lambda d=document: dd.require_handoff(d),
                    "cannot disagree",
                    "a handoff naming an artifact its result never retained")

    document = handoff_document()
    document["artifacts"] = []
    expect_rejected(lambda d=document: dd.require_handoff(d),
                    "cannot disagree",
                    "a handoff dropping the artifacts its result retained")

    document = handoff_document()
    document["invocation"]["ports"] = [
        port + 1 for port in document["invocation"]["ports"]]
    expect_rejected(lambda d=document: dd.require_handoff(d),
                    "describes runs that did not happen",
                    "a handoff whose ports are not its runs' own")

    document = handoff_document()
    document["invocation"]["ports"] = list(
        reversed(document["invocation"]["ports"]))
    expect_rejected(lambda d=document: dd.require_handoff(d),
                    "in that order",
                    "a handoff whose ports are its runs' in another order")


def test_the_handoff_manifest_defines_both_batches_configuration() -> None:
    """Not the incidental `config/` state when the diagnosis started.

    `Engine.Core.Init.migrateLegacyConfig` can materialize an absent local
    file during a first boot, so "what is there now" and "what the
    measurement read" are different questions. The handoff's manifest is
    the authority, and a batch that diverges from it did not reproduce
    the condition.
    """
    # Present in the handoff, absent from the batch.
    document = diagnosis_document(
        handoff=handoff_document(config=config_entries([(LOCAL_YAML,
                                                         DIGEST_A)])))
    expect_refused(lambda d=document: evaluate(d), "is absent from",
                   "a baseline missing a file the handoff recorded")

    # Absent from the handoff, present in the batch — absence matches as
    # rigorously as contents, or the extra file is an unrecorded condition.
    document = diagnosis_document()
    document["baseline"]["configuration"] = manifest([(LOCAL_YAML, DIGEST_A)])
    expect_refused(lambda d=document: evaluate(d), "is absent from",
                   "a baseline carrying a file the handoff never recorded")

    # Present in both, different bytes.
    document = diagnosis_document(
        handoff=handoff_document(config=config_entries([(LOCAL_YAML,
                                                         DIGEST_A)])))
    document["baseline"]["configuration"] = manifest([(LOCAL_YAML, DIGEST_B)])
    document["verification"]["configuration"] = manifest([(LOCAL_YAML,
                                                           DIGEST_B)])
    expect_refused(lambda d=document: evaluate(d), "differs",
                   "a baseline whose recorded bytes do not match")

    # An empty manifest on both sides is the expected default, not a gap.
    agreeing = diagnosis_document()
    expect(evaluate(agreeing).route == dd.ROUTE_REPAIR,
           "two confirmed-empty manifests are the same condition")


def test_unrecoverable_configuration_bytes_are_the_cannot_reproduce_route()\
        -> None:
    """The condition could not be established, which is a RESULT.

    The approved correction routes it to #1439 with its evidence rather
    than rejecting it: the invocation ran and found it could not recreate
    the bytes, which is exactly what that route reports.
    """
    document = diagnosis_document(
        route=dd.ROUTE_CANNOT_REPRODUCE,
        handoff=handoff_document(config=config_entries([(LOCAL_YAML,
                                                         DIGEST_A)])))
    document.pop("verification", None)
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_CANNOT_REPRODUCE,
           f"an unrecoverable configuration is that outcome; got "
           f"{outcome.route}")
    expect(outcome.owner_issue == 1439,
           f"owned by #1439; got {outcome.owner_issue}")
    expect("configuration state" in outcome.detail,
           f"and it says why: {outcome.detail}")

    # Declared as a repair, the same evidence names the route it should be.
    repair = diagnosis_document(
        handoff=handoff_document(config=config_entries([(LOCAL_YAML,
                                                         DIGEST_A)])))
    expect_refused(lambda: evaluate(repair), "cannot-reproduce",
                   "a repair over a configuration that could not be recreated")


def test_a_batch_must_hold_the_probes_declared_resource_interests() -> None:
    """`peak_concurrency: 1` cannot prove cross-process isolation.

    It counts other flake-harness invocations only; an independent
    `run_probes.py` sweep holding the same repository-relative resource
    never appears in it. `probe_resource_lock` is what coordinates across
    processes, so the batch has to have held the probe's own declared
    interests.
    """
    for section in ("baseline", "verification"):
        absent = diagnosis_document()
        del absent[section]["resource_hold"]
        expect_rejected(lambda d=absent: evaluate(d),
                        "records no `resource_hold`",
                        f"a {section} that recorded no resource hold")

        # The interests are the PROBE's, not the batch's to choose.
        narrowed = diagnosis_document()
        narrowed[section]["resource_hold"] = resource_hold(shared=[])
        expect_rejected(lambda d=narrowed: evaluate(d),
                        "the probe's own, not this batch's to choose",
                        f"a {section} declaring fewer interests than {PROBE}")

        invented = diagnosis_document()
        invented[section]["resource_hold"] = resource_hold(
            exclusive=["a-resource-nobody-declared"])
        expect_rejected(lambda d=invented: evaluate(d),
                        "the probe's own, not this batch's to choose",
                        f"a {section} inventing an exclusive interest")

        # A hold taken AFTER the configuration was installed leaves the
        # manifest describing a state the runs never saw.
        late = diagnosis_document()
        late[section]["resource_hold"] = resource_hold(covers=False)
        expect_rejected(lambda d=late: evaluate(d),
                        "covered the configuration install",
                        f"a {section} hold that started after the install")


def test_a_busy_resource_hold_is_a_measurement_that_did_not_happen() -> None:
    """Another process owned it, so the batch was never controlled.

    Reported as a batch problem and routed to #1439, not raised: the
    documents are well-formed and the invocation really ran — it simply
    did not run under the conditions the comparison assumes.
    """
    document = diagnosis_document(route=dd.ROUTE_CANNOT_REPRODUCE)
    document["baseline"]["resource_hold"] = resource_hold(
        held=False, detail="held by an independent run_probes.py sweep")
    document.pop("verification", None)
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_CANNOT_REPRODUCE,
           f"a contended baseline is cannot-reproduce; got {outcome.route}")
    expect("run_probes.py sweep" in outcome.detail,
           f"and the reason is carried, not summarised away: {outcome.detail}")

    repair = diagnosis_document()
    repair["baseline"]["resource_hold"] = resource_hold(
        held=False, detail="held by an independent run_probes.py sweep")
    expect_refused(lambda: evaluate(repair), "not a usable measurement",
                   "a repair built on a contended baseline")


def test_the_emitted_outcome_carries_every_declared_field() -> None:
    """#1437 owns the PRODUCER record; #1438/#1439 own consumption.

    The approved spec addition enumerates what the one versioned
    diagnosis-result artifact must carry. Asserted field by field against
    that list rather than against a snapshot, so a field that silently
    stopped being populated fails here instead of downstream.
    """
    document = diagnosis_document()
    emitted = evaluate(document).to_document()

    expect(emitted["schema"] == dd.OUTCOME_SCHEMA,
           f"a stable schema: {emitted['schema']}")
    expect(emitted["route"] == dd.ROUTE_REPAIR,
           f"a stable route identifier: {emitted['route']}")

    handoff = document["handoff"]
    identity = emitted["handoff"]
    expect(identity is not None, "the input handoff is identified")
    # The census row cannot answer these, which is why they are here.
    for field, expected in (
            ("probe", handoff["probe"]),
            ("commit_sha", handoff["result"]["commit_sha"]),
            ("acceptable_failures", handoff["acceptable_failures"]),
            ("targets", handoff["targets"]),
            ("timestamp_utc", handoff["result"]["timestamp_utc"]),
            ("artifact_root", handoff["result"]["artifact_root"]),
            ("invocation_dir", handoff["result"]["invocation_dir"]),
            ("command", handoff["invocation"]["argv"]),
            ("directory", handoff["invocation"]["cwd"]),
            ("retained_artifacts", handoff["artifacts"])):
        expect(identity[field] == expected,
               f"the handoff identity carries {field}: "
               f"{identity[field]!r} vs {expected!r}")

    expect(emitted["baseline_sha"] == handoff["result"]["commit_sha"],
           f"the baseline SHA: {emitted['baseline_sha']}")
    expect(emitted["acceptable_failures"] == handoff["acceptable_failures"],
           f"X: {emitted['acceptable_failures']}")
    expect(emitted["configuration"]["entries"]
           == handoff["configuration"],
           f"the configuration manifest: {emitted['configuration']}")

    for label in ("baseline", "verification"):
        reference = emitted[label]
        expect(reference is not None, f"the {label} is referenced")
        result = document[label]["result"]
        for field in ("commit_sha", "artifact_root", "invocation_dir",
                      "retained_artifacts"):
            expect(reference[field] == result[field],
                   f"the {label} reference carries {field}: "
                   f"{reference[field]!r} vs {result[field]!r}")
        expect(reference["worktree"] == document[label]["worktree"],
               f"and the {label} worktree it ran in")

    expect(emitted["diagnosis"] == document["diagnosis"],
           "the diagnosis evidence rides along")
    expect(emitted["attestations"] == document["attestations"],
           "so do the preservation attestations")
    expect(emitted["repair"] == document["repair"],
           "and the repair's commit evidence")

    # A route with no batches leaves the optional halves explicitly null
    # rather than dropping the keys, so a consumer reads one shape.
    passing = handoff_document(result=result_document())
    quiet = evaluate(diagnosis_document(
        route=dd.ROUTE_NO_TARGET, handoff=passing, baseline=False,
        verification=False)).to_document()
    for field in ("baseline", "verification", "repair", "attestations"):
        expect(field in quiet and quiet[field] is None,
               f"the no-target outcome states {field} as null; got "
               f"{quiet.get(field)!r}")
    expect(quiet["handoff"] is not None and quiet["baseline_sha"],
           "while still identifying the handoff it consumed")


def test_a_missing_target_qualifies_for_repair_below_x() -> None:
    """A batch can be clean by the numbers and still have lost a check.

    `probe_protocol.parse_event_stream` represents a declared check that
    was never emitted as MISSING, while `probe_flake.reconcile` classifies
    a zero-exit run carrying no FAIL event as PASS. So the run outcome is
    PASS, the aggregate failure count is 0, and the target check was not
    observed at all — which the approved correction says qualifies for
    repair independently of the aggregate arithmetic.
    """
    # Every run PASSes as a RUN while `gamma` is never emitted.
    ids = [cid for cid, _label in CHECKS]
    lost = [{cid: (MISSING if cid == "gamma" else PASS) for cid in ids}
            for _ in range(dd.RUN_COUNT)]
    baseline = result_document(runs=lost)
    expect(dd.failure_count(baseline) == 0,
           f"the batch is clean by the numbers; got "
           f"{dd.failure_count(baseline)} failures")
    expect(dd.missing_targets(baseline, ("gamma",)) == ["gamma"],
           "and yet the target was never emitted")

    handoff = handoff_document(result=baseline, acceptable=1)
    expect(handoff["targets"] == ["gamma"],
           f"the producer derives the lost check as the target; got "
           f"{handoff['targets']}")

    document = diagnosis_document(handoff=handoff)
    document["baseline"]["result"] = baseline
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"a reproducibly MISSING target supports a repair below X; got "
           f"{outcome.route}")

    # And the same evidence refuses `cannot-reproduce`, which would be
    # claiming the batch showed nothing.
    denied = diagnosis_document(route=dd.ROUTE_CANNOT_REPRODUCE,
                                handoff=handoff)
    denied["baseline"]["result"] = baseline
    denied.pop("verification", None)
    expect_refused(lambda: evaluate(denied), "as MISSING",
                   "cannot-reproduce over a reproducibly MISSING target")


def test_verification_is_not_relaxed_by_the_missing_qualification() -> None:
    """Repair may START from a MISSING target; it may not END with one.

    The correction widened the pre-fix qualification only. Verification
    still has to come in at or below X AND satisfy the MISSING rules, so
    a repair whose verification still loses the target is not a repair.
    """
    ids = [cid for cid, _label in CHECKS]
    lost = [{cid: (MISSING if cid == "gamma" else PASS) for cid in ids}
            for _ in range(dd.RUN_COUNT)]
    handoff = handoff_document(result=result_document(runs=lost), acceptable=1)
    document = diagnosis_document(handoff=handoff)
    document["baseline"]["result"] = result_document(runs=lost)
    document["verification"]["result"] = verification_result(
        runs=lost, artifact_root=VERIFY_ARTIFACTS)
    expect_refused(lambda: evaluate(document), "MISSING",
                   "a verification that still loses the target")


def test_a_cannot_reproduce_outcome_names_the_baseline_it_ran() -> None:
    document = diagnosis_document(route=dd.ROUTE_CANNOT_REPRODUCE,
                                  handoff=handoff_document(acceptable=1))
    # Non-aborting, so no target is left MISSING: a MISSING target is a
    # reproduced defect and would refuse `cannot-reproduce` outright.
    document["baseline"]["result"] = result_document(
        runs=failing_runs(1, abort=False))
    document.pop("verification", None)
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_CANNOT_REPRODUCE, "the route holds")
    for path in document["baseline"]["result"]["retained_artifacts"]:
        expect(path in outcome.artifacts,
               f"the baseline's {path} is handed on; got {outcome.artifacts}")


def test_partial_improvement_is_refused_when_the_batch_was_accepted() -> None:
    document = diagnosis_document(route=dd.ROUTE_PARTIAL_IMPROVEMENT)
    expect_refused(lambda: evaluate(document), "an accepted verification",
                   "partial-improvement declared over a clean batch")


def test_handoff_rejected_is_never_declared_after_the_gate_passed() -> None:
    document = diagnosis_document(route=dd.ROUTE_HANDOFF_REJECTED)
    expect_rejected(lambda: evaluate(document), "never a conclusion drawn",
                   "handoff-rejected declared over an accepted handoff")


def test_an_unknown_route_is_refused() -> None:
    document = diagnosis_document()
    document["route"] = "probe-is-fine"
    expect_rejected(lambda: evaluate(document), "the declared routes are",
                    "an invented route")


def test_every_route_has_a_declared_owner() -> None:
    expect(set(dd.ROUTE_OWNER) == set(dd.ROUTES),
           "every route names its owning issue, or explicitly none")
    expect(dd.ROUTES_THAT_CHANGE_CODE == frozenset({dd.ROUTE_REPAIR}),
           "exactly one route may touch the probe's source")


# ==========================================================================
# Assertion weakening and the required evidence
# ==========================================================================
def test_a_repair_without_evidence_is_refused() -> None:
    for diagnosis, fragment in (
            (None, "states no diagnosis"),
            ({}, "records no diagnosis evidence"),
            ({"category": "observation"}, "records no diagnosis evidence"),
            ({"category": "observation", "evidence": []},
             "records no diagnosis evidence"),
            ({"category": "observation", "evidence": ["  "]},
             "records no diagnosis evidence"),
            ({"category": "observation", "evidence": [42]},
             "records no diagnosis evidence"),
    ):
        document = diagnosis_document()
        if diagnosis is None:
            del document["diagnosis"]
        else:
            document["diagnosis"] = diagnosis
        expect_rejected(lambda d=document: evaluate(d), fragment,
                        f"a repair whose diagnosis is {diagnosis!r}")


def test_a_repair_names_one_probe_side_cause_from_the_boundary() -> None:
    for category in (None, "the probe is racy", "production-code"):
        document = diagnosis_document()
        document["diagnosis"] = {"category": category,
                                 "evidence": ["the engine log"]}
        expect_rejected(lambda d=document: evaluate(d),
                        "one probe-side cause",
                        f"a repair whose cause is {category!r}")


def test_every_preservation_attestation_is_required() -> None:
    for name in dd.ATTESTATIONS:
        document = diagnosis_document()
        document["attestations"][name] = False
        expect_rejected(lambda d=document: evaluate(d), name,
                        f"a repair that did not attest {name}")
        document = diagnosis_document()
        del document["attestations"][name]
        expect_rejected(lambda d=document: evaluate(d), name,
                        f"a repair that omitted {name}")


def test_a_repair_may_not_change_production_code() -> None:
    document = diagnosis_document(repair={
        "commit_sha": REPAIR_COMMIT, "base_sha": BASE_COMMIT,
        "changed_paths": ["tools/role_probe.py", "src/Unit/Thread.hs"]})
    expect_rejected(lambda: evaluate(document),
                    "outside this workflow's repair scope",
                    "a repair that touched the engine")


def test_a_repair_may_extend_the_headless_suite() -> None:
    """Focused regression coverage is required, so it must be allowed."""
    document = diagnosis_document(repair={
        "commit_sha": REPAIR_COMMIT, "base_sha": BASE_COMMIT,
        "changed_paths": ["tools/role_probe.py",
                          "test-headless/Test/Headless/Role.hs"]})
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           "a probe repair may add deterministic coverage beside itself")


# ==========================================================================
# The repair is frozen before it is verified
# ==========================================================================
def test_a_dirty_repair_worktree_invalidates_the_verification() -> None:
    document = diagnosis_document()
    document["verification"]["source_clean"] = False
    expect_rejected(lambda: evaluate(document), "source-clean",
                    "a verification run against uncommitted source")


def test_the_verification_must_measure_the_proposed_commit() -> None:
    document = diagnosis_document()
    document["repair"]["commit_sha"] = "c" * 40
    expect_rejected(lambda: evaluate(document),
                    "measures something this pull request does not contain",
                    "a verification of another commit")


def test_a_repair_with_no_resolved_commit_is_refused() -> None:
    document = diagnosis_document(repair={"commit_sha": "HEAD",
                                          "base_sha": BASE_COMMIT,
                                          "changed_paths": ["tools/x.py"]})
    expect_rejected(lambda: evaluate(document),
                    "must be 40 lowercase hex characters",
                    "a repair with no commit")


def test_a_repair_with_no_changed_paths_is_refused() -> None:
    document = diagnosis_document(repair={"commit_sha": REPAIR_COMMIT,
                                          "base_sha": BASE_COMMIT,
                                          "changed_paths": []})
    expect_rejected(lambda: evaluate(document), "records no changed paths",
                    "a repair that changed nothing")


# ==========================================================================
# The one-PR limit
# ==========================================================================
def test_one_invocation_opens_at_most_one_pull_request() -> None:
    outcome = evaluate(diagnosis_document())
    session = dd.Diagnosis(outcome.handoff)
    expect(session.open_pull_request(outcome) == 1, "the first PR is allowed")
    try:
        session.open_pull_request(outcome)
    except dd.RouteRefused as error:
        expect("already opened a pull request" in str(error),
               f"the second PR is refused, got {error}")
    else:
        FAILURES.append("a second pull request was allowed")


def test_a_non_repair_route_opens_no_pull_request() -> None:
    document = diagnosis_document(route=dd.ROUTE_CANNOT_REPRODUCE)
    document["baseline"]["result"] = result_document()
    document.pop("verification", None)
    outcome = evaluate(document)
    session = dd.Diagnosis(outcome.handoff)
    try:
        session.open_pull_request(outcome)
    except dd.RouteRefused as error:
        expect("opens no pull request" in str(error),
               f"a non-repair route is refused a PR, got {error}")
    else:
        FAILURES.append("a cannot-reproduce route opened a pull request")


# ==========================================================================
# The CLI
# ==========================================================================
def _run_cli(*args) -> subprocess.CompletedProcess:
    return subprocess.run([sys.executable, TOOL, *args], text=True,
                          capture_output=True, timeout=120)


def _live_document(**kwargs) -> dict:
    """A diagnosis the CLI's OWN worktree resolution will accept.

    The CLI derives the primary checkout from `git worktree list`, so a
    fixture that invented one would exercise nothing but the refusal.
    Everything else — the comparison worktrees, the evidence paths — stays
    synthetic and outside every real worktree.
    """
    document = diagnosis_document(**kwargs)
    live = dd.primary_checkout()
    document["handoff"]["invocation"] = deflake_invocation(
        cmd=[f"{live}/tools/deflake.py", "--json",
             "--result", f"{OUTSIDE}/handoff.json"],
        directory=str(live))
    return document


def test_a_path_no_filesystem_can_name_is_refused_not_a_traceback() -> None:
    """`probe-flake-result/v1` says "string"; the filesystem says more.

    A path carrying an embedded NUL is schema-valid and makes
    `Path.resolve()` raise `ValueError` out of `lstat` — not `OSError`,
    which the containment helper already tolerated, and not this module's
    own exception, which is all `main` catches. So the CLI printed a
    traceback where `handoff-rejected` was the required answer.

    NB the NUL only ever arrives inside a DOCUMENT: `subprocess` refuses
    an argv token containing one, so `--manifest` cannot be handed such a
    root by any real caller.
    """
    for field, place in (("artifact_root", "the result's artifact root"),
                         ("invocation_dir", "the result's invocation dir")):
        document = handoff_document()
        document["result"][field] = "/tmp/\x00"
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "contains a NUL", f"{place} carrying a NUL")

    # A run's own directory, kept consistent with the retention rule so
    # the NUL is what fails rather than the envelope's equality.
    document = handoff_document()
    unnameable = "/tmp/a\x00b"
    document["result"]["runs"][0]["artifact_dir"] = unnameable
    document["result"]["retained_artifacts"][0] = unnameable
    document["artifacts"][0] = unnameable
    expect_rejected(lambda d=document: dd.require_handoff(d),
                    "contains a NUL", "a run's artifact dir carrying a NUL")

    # Through the real CLI, because "does not raise" and "exits the way
    # this tool is specified to exit" are different claims.
    root = Path(tempfile.mkdtemp(prefix="test_deflake_diagnosis_nul_"))
    try:
        document = handoff_document()
        document["result"]["artifact_root"] = "/tmp/\x00"
        path = root / "handoff.json"
        path.write_text(json.dumps(document), encoding="utf-8")
        done = _run_cli("--handoff", str(path))
        expect(done.returncode == dd.EXIT_REJECTED,
               f"an unnameable path exits {dd.EXIT_REJECTED}; got "
               f"{done.returncode}")
        expect("Traceback" not in done.stderr,
               f"without a traceback: {done.stderr[:200]}")
        expect(dd.ROUTE_HANDOFF_REJECTED in done.stderr,
               f"naming the route: {done.stderr[:200]}")

        # The same through the diagnosis entry point.
        document = _live_document()
        document["baseline"]["result"]["artifact_root"] = "/tmp/\x00"
        path = root / "diagnosis.json"
        path.write_text(json.dumps(document), encoding="utf-8")
        done = _run_cli("--diagnosis", str(path))
        expect(done.returncode == dd.EXIT_REJECTED,
               f"and so does a diagnosis carrying one; got {done.returncode}")
        expect("Traceback" not in done.stderr,
               f"without a traceback: {done.stderr[:200]}")
    finally:
        shutil.rmtree(root, ignore_errors=True)

    # A NUL in a recorded COMMAND destination reaches the containment
    # helper instead, which `_require_canonical` never sees — so
    # `_path_forms` has to stay total for it. Without that, this is an
    # uncaught ValueError rather than a refusal.
    document = diagnosis_document()
    command_tokens = list(document["baseline"]["invocation"]["command"])
    command_tokens[command_tokens.index("--artifact-root") + 1] = "/tmp/\x00a"
    document["baseline"]["invocation"]["command"] = command_tokens
    raised = None
    try:
        evaluate(document)
    except (dd.HandoffError, dd.RouteRefused) as error:
        raised = error
    except Exception as error:                      # noqa: BLE001
        expect(False, f"an unnameable destination escaped as "
                      f"{type(error).__name__}: {error}")
    expect(raised is not None,
           "a command destination carrying a NUL is refused")

    # And the helper is total for it directly.
    forms = dd._path_forms("/tmp/\x00a")
    expect(bool(forms),
           f"_path_forms answers for an unnameable path: {forms}")

    # The helper itself, on the two shapes a document can carry.
    for value in (None, 42, "", "/tmp/\x00"):
        raised = False
        try:
            dd.require_path(value, "a path")
        except dd.HandoffError:
            raised = True
        expect(raised, f"require_path refuses {value!r}")
    expect(dd.require_path("/tmp/fine", "a path") == "/tmp/fine",
           "and returns a usable one unchanged")


def test_the_cli_reports_the_route_and_its_exit_status() -> None:
    root = Path(tempfile.mkdtemp(prefix="test_deflake_diagnosis_"))
    try:
        path = root / "diagnosis.json"
        path.write_text(json.dumps(_live_document()), encoding="utf-8")
        done = _run_cli("--diagnosis", str(path), "--json")
        expect(done.returncode == dd.EXIT_OK,
               f"an accepted repair exits {dd.EXIT_OK}: {done.stderr}")
        document = json.loads(done.stdout)
        expect(document["route"] == dd.ROUTE_REPAIR, "and names the route")
        expect(document["schema"] == dd.OUTCOME_SCHEMA,
               "in the outcome schema")
        expect(document["opens_pull_request"] is True,
               "declaring that it opens the pull request")

        broken = root / "broken.json"
        document = _live_document()
        document["handoff"]["probe"] = [PROBE, OTHER]
        broken.write_text(json.dumps(document), encoding="utf-8")
        done = _run_cli("--diagnosis", str(broken))
        expect(done.returncode == dd.EXIT_REJECTED,
               f"a rejected handoff exits {dd.EXIT_REJECTED}")
        expect(dd.ROUTE_HANDOFF_REJECTED in done.stderr,
               f"naming the route: {done.stderr}")

        refused = root / "refused.json"
        document = _live_document()
        document["baseline"]["result"] = result_document()
        refused.write_text(json.dumps(document), encoding="utf-8")
        done = _run_cli("--diagnosis", str(refused))
        expect(done.returncode == dd.EXIT_REFUSED,
               f"a denied route exits {dd.EXIT_REFUSED}")

        gate = root / "handoff.json"
        gate.write_text(json.dumps(_live_document()["handoff"]),
                        encoding="utf-8")
        done = _run_cli("--handoff", str(gate))
        expect(done.returncode == dd.EXIT_OK,
               f"the entry gate alone exits 0: {done.stderr}")
        expect(PROBE in done.stdout, "naming the probe it accepted")

        done = _run_cli("--manifest", str(root))
        expect(done.returncode == dd.EXIT_OK, "a manifest run exits 0")
        expect(json.loads(done.stdout)["entries"] == [],
               "and states an absent configuration family explicitly")
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_the_cli_needs_exactly_one_mode() -> None:
    done = _run_cli()
    expect(done.returncode != 0, "no mode at all is an error")
    done = _run_cli("--diagnosis", "a.json", "--handoff", "b.json")
    expect(done.returncode != 0, "two modes at once is an error")


def test_a_malformed_document_reaches_the_cli_as_a_rejection() -> None:
    root = Path(tempfile.mkdtemp(prefix="test_deflake_diagnosis_"))
    try:
        for label, mutate in (
                ("expected_checks", lambda d: d["handoff"].update(
                    {"expected_checks": 42})),
                ("targets", lambda d: d["handoff"].update({"targets": 42})),
                ("timestamp_utc", lambda d: d["handoff"]["result"].update(
                    {"timestamp_utc": "2026-99-99T99:99:99Z"})),
                ("artifact_root", lambda d: d["handoff"]["result"].update(
                    {"artifact_root": f"{OUTSIDE}/a/../b"})),
        ):
            document = _live_document()
            mutate(document)
            path = root / f"malformed-{label}.json"
            path.write_text(json.dumps(document), encoding="utf-8")
            done = _run_cli("--diagnosis", str(path))
            expect(done.returncode == dd.EXIT_REJECTED,
                   f"a malformed {label} exits {dd.EXIT_REJECTED}, got "
                   f"{done.returncode}: {done.stderr}")
            expect("Traceback" not in done.stderr,
                   f"without a traceback: {done.stderr}")
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_an_unreadable_document_is_a_rejection_not_a_traceback() -> None:
    done = _run_cli("--diagnosis", "/nonexistent/diagnosis.json")
    expect(done.returncode == dd.EXIT_REJECTED,
           f"a missing document exits {dd.EXIT_REJECTED}")
    expect("Traceback" not in done.stderr,
           f"without a traceback: {done.stderr}")


# ==========================================================================
# Constants that must not drift
# ==========================================================================
def test_the_measurement_contract_comes_from_its_owners() -> None:
    expect(dd.RUN_COUNT == probe_census.POLICY_RUN_COUNT,
           "the run count is the census policy's own N")
    expect(dd.RTS_CAPABILITIES == probe_flake.DEFAULT_RTS_CAPS,
           "the capability count is the harness's own default")
    expect(dd.RUN_COUNT == 10 and dd.RTS_CAPABILITIES == 4,
           "and both are what #1436 measured under")


def test_x_arithmetic_is_delegated_rather_than_reimplemented() -> None:
    """Guards against a second copy of `failures <= X` drifting from the census."""
    source = Path(dd.__file__).read_text(encoding="utf-8")
    expect("probe_census.tolerance_state" in source,
           "the tolerance comparison is the census policy's own")
    expect("probe_census.require_acceptable_failures" in source,
           "and so is X's validation")


# ==========================================================================
# Mutation evidence
# ==========================================================================
# Every provenance invariant below is asserted twice: once by a rejection
# test above, and once here by NEUTRALISING exactly that invariant and
# proving the same fixture is then ACCEPTED.
#
# The second half is what makes the first half evidence. A rejection test
# passes just as happily when a DIFFERENT rule is what did the rejecting
# — the fixture that violates an interpreter floor also, quite often,
# violates a path binding — and then the invariant it claims to cover
# could be deleted without a single test turning red. Bypassing one rule
# at a time is the only way to show which rule each case actually holds.
#
# The bypass is a TEXTUAL edit to a private copy of the module source,
# compiled into a throwaway module. Nothing in the shipped module exists
# to support it: a production hook a test could flip would be a second
# code path in the thing under test, which is exactly what this suite is
# for catching.
def mutant(anchor: str, replacement: str):
    """`deflake_diagnosis` with one invariant neutralised, as a module.

    The anchor must appear EXACTLY once. A silently-missed replacement
    would produce an unmodified module, whose faithful rejection would
    then read as "the bypass changed nothing" — evidence for the
    invariant where none was gathered — so a drifted anchor is a loud
    failure rather than a quiet pass.
    """
    source = Path(dd.__file__).read_text(encoding="utf-8")
    found = source.count(anchor)
    if found != 1:
        raise AssertionError(
            f"the mutation anchor appears {found} times, not once: "
            f"{anchor!r}. It has drifted from the module and this case is "
            f"gathering no evidence.")
    module = types.ModuleType(f"deflake_diagnosis_mutant_{abs(hash(anchor))}")
    module.__file__ = dd.__file__
    exec(compile(source.replace(anchor, replacement), dd.__file__, "exec"),
         module.__dict__)
    return module


def _through_evaluate(module, document):
    return module.evaluate(document, worktrees=WORKTREES, primary=PRIMARY_WT)


def _through_gate(module, document):
    return module.require_handoff(document)


def _refusal(module, document, run):
    """The message `module` refuses `document` with, or `None`."""
    try:
        run(module, document)
    except (module.HandoffError, module.RouteRefused) as error:
        return str(error)
    return None


def check_mutation(label, fragment, anchor, replacement, build,
                   run=_through_evaluate):
    """One invariant, held to both halves of the mutation contract.

    `build` returns a fresh document violating this invariant, and
    `fragment` is what the rejection test above asserts. The shipped
    module must refuse it naming that fragment; the module with this one
    rule bypassed must NOT — which is precisely "bypassing only this
    invariant makes its rejection test fail".

    Usually the bypassed module accepts the document outright. Where one
    invariant is nested inside a broader one — two batches that share an
    invocation directory are also, necessarily, each writing inside the
    other's — the bypassed module refuses for the OTHER reason, and the
    rejection test still fails because it is asserting on the message.
    Both outcomes are reported distinctly so a reader can tell which
    happened.
    """
    document = build()
    refusal = _refusal(dd, document, run)
    if refusal is None:
        FAILURES.append(f"{label}: the shipped module ACCEPTED the fixture, "
                        f"so this case proves nothing")
        return
    if fragment not in refusal:
        FAILURES.append(f"{label}: the shipped module refused the fixture for "
                        f"{refusal!r} rather than {fragment!r}, so the "
                        f"fixture does not isolate this invariant")
        return
    try:
        bypassed = mutant(anchor, replacement)
    except AssertionError as error:
        FAILURES.append(f"{label}: {error}")
        return
    after = _refusal(bypassed, build(), run)
    if after is not None and fragment in after:
        FAILURES.append(
            f"{label}: with this invariant bypassed the fixture is still "
            f"refused for {after!r}, so the rejection test above is held up "
            f"by some OTHER rule and this invariant is unevidenced")


def _relaunched(program: str) -> dict:
    """Both controlled commands relaunched under `program`."""
    document = diagnosis_document()
    for batch in ("baseline", "verification"):
        recorded = document[batch]["invocation"]
        recorded["command"] = [program] + recorded["command"][1:]
    return document


def _baseline_command(*tokens) -> dict:
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(cmd=list(tokens))
    return document


def _relocate_artifacts(result, old: str, new: str) -> dict:
    """Respell every artifact-derived path in `result`, consistently.

    A fixture that changed one of them would violate the topology rule
    as well as whatever it meant to test, and the mutation harness would
    correctly report that it proves nothing.
    """
    result["artifact_root"] = result["artifact_root"].replace(old, new)
    result["invocation_dir"] = result["invocation_dir"].replace(old, new)
    for run in result["runs"] + ([result["error_run"]]
                                 if result.get("error_run") else []):
        if run.get("artifact_dir"):
            run["artifact_dir"] = run["artifact_dir"].replace(old, new)
    result["retained_artifacts"] = [entry.replace(old, new)
                                    for entry in result["retained_artifacts"]]
    return result


def _command_without(option: str) -> list:
    tokens = command()
    index = tokens.index(option)
    return tokens[:index] + tokens[index + 2:]


def _require_result(module, document):
    return module.require_result(document, "a result")


def _deflake_argv_without_json(*extra) -> dict:
    document = handoff_document()
    document["invocation"]["argv"] = [
        token for token in document["invocation"]["argv"]
        if token != "--json"] + list(extra)
    return document


def test_the_interpreter_floor_is_what_refuses_an_old_interpreter() -> None:
    check_mutation(
        "the 3.10 syntax floor",
        "below this lab's 3.10 syntax floor",
        "    if minor is not None and _below(minor, "
        "INTERPRETER_MINOR_FLOOR):",
        "    if False:",
        lambda: _relaunched("python3.9"))


def test_the_interpreter_grammar_is_what_refuses_another_program() -> None:
    check_mutation(
        "the interpreter token grammar",
        "is not a Python 3 interpreter token",
        "    matched = INTERPRETER_RE.fullmatch(program)",
        '    matched = INTERPRETER_RE.fullmatch("python3")',
        lambda: _relaunched("pypy"))


def test_the_bare_name_rule_is_what_refuses_a_path_qualified_interpreter()\
        -> None:
    """Bypassing it means reading the token as the bare name it ends with.

    The two interpreter rules cannot be separated by choosing a cleverer
    fixture — no path-qualified token satisfies the grammar, because the
    grammar admits no separator — so the bypass is what isolates them:
    with the path rule gone, `/tmp/counterfeit/python3` is `python3`.
    """
    check_mutation(
        "the bare-interpreter-name rule",
        "runs the interpreter by path",
        "    if os.sep in program or (os.altsep and os.altsep in program):",
        "    program = os.path.basename(program)\n    if False:",
        lambda: _relaunched("/tmp/counterfeit/python3"))


def test_the_script_binding_is_what_refuses_a_counterfeit_tool() -> None:
    check_mutation(
        "the script-to-checkout binding",
        "the checkout it declares keeps that tool at",
        "        if not (_path_forms(script, base if base is not None "
        "else root)\n                & _path_forms(expected)):",
        "        if False:",
        lambda: _baseline_command(
            "python3", "/tmp/counterfeit/probe_flake.py", *command()[2:]))


def test_the_option_surface_is_what_refuses_an_invented_option() -> None:
    check_mutation(
        "the closed option surface",
        "does not accept",
        "        if name not in found.options:",
        "        if False:",
        lambda: _baseline_command(*command(), "--timeout", "600"))


def test_the_positional_rule_is_what_refuses_a_bare_token() -> None:
    """Bypassing it means silently ignoring the token instead.

    Reading it as an option name would only move the refusal to the
    closed option surface, which is a different invariant with its own
    case above.
    """
    check_mutation(
        "the no-positionals rule",
        "positional token",
        '        if not token.startswith("--"):\n'
        '            raise HandoffError(',
        '        if not token.startswith("--"):\n'
        '            index += 1\n'
        '            continue\n'
        '        if False:\n'
        '            raise HandoffError(',
        lambda: _baseline_command(*command(), "extra"))


def test_the_arity_rule_is_what_refuses_a_value_on_a_flag() -> None:
    check_mutation(
        "flag arity",
        "which is a flag",
        '            if inline:\n'
        '                raise HandoffError(\n'
        '                    f"{what} passes a value to {name}, which is a '
        'flag")',
        '            if inline:\n'
        '                pass',
        lambda: _deflake_argv_without_json("--json=yes"),
        run=_through_gate)


def test_the_duplicate_rule_is_what_refuses_a_repeated_option() -> None:
    check_mutation(
        "duplicate-option rejection",
        "repeats",
        '            raise HandoffError(f"{what} repeats {name}")',
        "            pass",
        lambda: _baseline_command(*command(), "--runs", str(dd.RUN_COUNT)))


def test_the_missing_value_rule_is_what_refuses_a_dangling_option() -> None:
    """The dangling option is one the command does not already carry.

    Repeating an option it does have would be refused as a duplicate — a
    different invariant, with its own case above.
    """
    check_mutation(
        "a value-taking option with no value",
        "has no value",
        '                raise HandoffError(f"{what}: {token} has no value")',
        "                break",
        lambda: _baseline_command(*_command_without("--artifact-root"),
                                  "--artifact-root"))


def test_the_required_rule_is_what_refuses_a_command_with_no_result() -> None:
    check_mutation(
        "required options",
        "names no --result",
        "    for option in found.required:",
        "    for option in ():",
        lambda: _baseline_command(*_command_without("--result")))


def test_the_positive_rule_is_what_refuses_a_zero_run_count() -> None:
    """Bypassed, the same record surfaces as a condition disagreement.

    That is the exact failure the rule exists to prevent: a route
    reporting that two measurements disagreed, when one of them was
    never run.
    """
    check_mutation(
        "the producer's positive-value constraint",
        "must be a positive count",
        "    if positive and number < 1:",
        "    if False:",
        _zero_run_batch)


def test_the_integer_grammar_is_what_refuses_a_float_run_count() -> None:
    check_mutation(
        "argparse's own `int()` grammar",
        "must be an integer",
        "        number = int(value)",
        "        number = int(float(value))",
        lambda: _baseline_command(*_command_without("--runs"),
                                  "--runs", f"{dd.RUN_COUNT}.0"))


def test_canonical_spelling_is_what_refuses_a_traversal_path() -> None:
    """Every artifact path is respelled together, so only spelling differs.

    Changing one of them alone would move the invocation directory out
    from under its own root, and the topology rule — not this one —
    would be what refused the fixture.
    """
    check_mutation(
        "resolve-canonical artifact paths",
        "which is not the spelling",
        "    if resolved != path:",
        "    if False:",
        lambda: handoff_document(result=_relocate_artifacts(
            result_document(runs=failing_runs(3)),
            f"{OUTSIDE}/artifacts", f"{OUTSIDE}/forged/../artifacts")),
        run=_through_gate)


def test_the_direct_child_rule_is_what_refuses_a_nested_invocation() -> None:
    check_mutation(
        "invocation_dir is a direct child of artifact_root",
        "DIRECT child of the root",
        "    if invocation.parent != root:",
        "    if False:",
        lambda: handoff_document(result=_relocate_artifacts(
            result_document(runs=failing_runs(3)),
            f"{OUTSIDE}/artifacts/{PROBE}",
            f"{OUTSIDE}/artifacts/deeper/{PROBE}")),
        run=_through_gate)


def test_the_generated_name_rule_is_what_refuses_a_hand_made_directory()\
        -> None:
    check_mutation(
        "the generated invocation-directory name",
        "not a directory this measurement created",
        "    problem = invocation_name_problem(invocation.name, "
        'document["probe"])',
        "    problem = None",
        lambda: handoff_document(result=_relocate_artifacts(
            result_document(runs=failing_runs(3)),
            f"{PROBE}-20260821T120000Z-4711-abcdef12", f"{PROBE}-evidence")),
        run=_through_gate)


def test_the_run_index_rule_is_what_refuses_a_reordered_batch() -> None:
    """The records are SWAPPED whole, so each keeps its own `run-NNN`.

    Swapping only the index fields would put run 2's record in run 1's
    directory, and the topology rule would refuse it first.
    """
    def build():
        result = result_document(runs=failing_runs(3))
        result["runs"][0], result["runs"][1] = (result["runs"][1],
                                                result["runs"][0])
        result["retained_artifacts"] = [run["artifact_dir"]
                                        for run in result["runs"]
                                        if run["artifact_dir"]]
        return result

    check_mutation("run indices are 1..len(runs)",
        "numbers its runs",
                   "    if indices != expected:", "    if False:",
                   build, run=_require_result)


def test_the_error_index_rule_is_what_refuses_a_stray_error_run() -> None:
    """The error run's own directory moves with its index.

    Leaving the directory behind would collide with a completed run's,
    and the topology rule would be what refused the fixture.
    """
    def build():
        result = result_document(runs=failing_runs(3), harness_error=True)
        broken = result["error_run"]
        stale = broken["artifact_dir"]
        broken["index"] = len(result["runs"]) + 2
        broken["artifact_dir"] = str(Path(result["invocation_dir"])
                                     / f"run-{broken['index']:03d}")
        result["retained_artifacts"] = [
            broken["artifact_dir"] if entry == stale else entry
            for entry in result["retained_artifacts"]]
        return result

    check_mutation(
        "the harness-error run's index",
        "numbers its harness-error run",
        '    if isinstance(broken, dict) and broken["index"] != len(expected) '
        '+ 1:',
        "    if False:",
        build, run=_require_result)


def test_the_run_directory_rule_is_what_refuses_a_foreign_artifact_dir()\
        -> None:
    def build():
        result = result_document(runs=failing_runs(3))
        run = result["runs"][0]
        elsewhere = f"{OUTSIDE}/artifacts/elsewhere/run-001"
        result["retained_artifacts"] = [
            elsewhere if entry == run["artifact_dir"] else entry
            for entry in result["retained_artifacts"]]
        run["artifact_dir"] = elsewhere
        return result

    check_mutation("every run directory is `invocation_dir/run-NNN`",
        "every run directory is",
                   "        if Path(directory) != expected:",
                   "        if False:",
                   build, run=_require_result)


def test_the_retention_rule_is_what_refuses_a_kept_passing_run() -> None:
    """The kept directory is NOT declared, so only this rule is violated.

    Declaring it too would break the retained-list equality, which has
    its own case below.
    """
    def build():
        result = result_document(runs=failing_runs(3))
        for run in result["runs"]:
            if run["outcome"] == probe_flake.RUN_PASS:
                run["artifact_dir"] = str(Path(result["invocation_dir"])
                                          / f"run-{run['index']:03d}")
                break
        return result

    check_mutation(
        "a passing run keeps nothing",
        "passed and still names the artifact directory",
        "            if directory is not None:", "            if False:",
        build, run=_require_result)


def test_the_retention_rule_is_what_refuses_a_discarded_failure() -> None:
    def build():
        result = result_document(runs=failing_runs(3))
        for run in result["runs"]:
            if run["artifact_dir"]:
                result["retained_artifacts"] = [
                    entry for entry in result["retained_artifacts"]
                    if entry != run["artifact_dir"]]
                run["artifact_dir"] = None
                break
        return result

    check_mutation(
        "an unsuccessful run keeps everything",
        "a failure whose logs are gone",
        '        if directory is None:\n'
        '            raise HandoffError(\n'
        '                f"{where} did not pass',
        '        if directory is None:\n'
        '            continue\n'
        '        if False:\n'
        '            raise HandoffError(\n'
        '                f"{where} did not pass',
        build, run=_require_result)


def test_the_retained_list_rule_is_what_refuses_a_shuffled_list() -> None:
    """Ordered equality: a shuffled list names the same set, in no order.

    The bypass compares the two as SETS, which is exactly the weakening
    this rule exists to refuse — `Measurement.retained_artifacts` builds
    the list in run order and the error run comes last.
    """
    def build():
        result = result_document(runs=failing_runs(3))
        result["retained_artifacts"] = list(
            reversed(result["retained_artifacts"]))
        return result

    check_mutation(
        "retained_artifacts is the ORDERED list of kept directories",
        "naming evidence it does not have",
        "    if list(declared) != retained:",
        "    if sorted(declared) != sorted(retained):",
        build, run=_require_result)


def test_the_commit_delegation_is_what_refuses_a_placeholder_identity()\
        -> None:
    check_mutation(
        "commit identity via probe_census",
        "is the placeholder 'unknown'",
        "        return probe_census.require_commit_identity(value, what)",
        "        return value",
        lambda: result_document(runs=failing_runs(3), commit="unknown"),
        run=_require_result)


def test_the_timestamp_delegation_is_what_refuses_a_shaped_non_instant()\
        -> None:
    def build():
        result = result_document(runs=failing_runs(3))
        result["timestamp_utc"] = "2026-99-99T99:99:99Z"
        return result

    check_mutation(
        "timestamp via probe_census.parse_timestamp",
        "records WHEN its baseline was measured",
        '        probe_census.parse_timestamp(document.get("timestamp_utc"),',
        '        _ = 0 and probe_census.parse_timestamp('
        'document.get("timestamp_utc"),',
        build, run=_require_result)


def test_the_schema_delegation_is_what_refuses_an_impossible_result() -> None:
    """A run that says PASS while its own check map carries a FAIL.

    `probe_census.validate_result`'s rule and nothing local — which is
    what makes the delegation load-bearing rather than decorative.
    """
    def build():
        result = result_document(runs=failing_runs(3))
        for run in result["runs"]:
            if run["outcome"] == probe_flake.RUN_FAIL:
                run["outcome"] = probe_flake.RUN_PASS
                result["retained_artifacts"] = [
                    entry for entry in result["retained_artifacts"]
                    if entry != run["artifact_dir"]]
                run["artifact_dir"] = None
                break
        return result

    check_mutation(
        "probe_census.validate_result",
        "PASS",
        "        probe_census.validate_result(document)",
        "        pass",
        build, run=_require_result)


def test_the_containment_rule_is_what_refuses_a_shared_invocation_dir()\
        -> None:
    def build():
        """Both batches default their root, so only the sharing is wrong.

        Pointing the verification at the baseline's invocation directory
        while it declared its own root would put that directory outside
        the root it reports, and the direct-child rule would be what
        refused the fixture.
        """
        def defaulted(section, tree, result):
            section["invocation"] = invocation(
                cmd=["python3", f"{tree}/tools/probe_flake.py",
                     "--probe", PROBE, "--runs", str(dd.RUN_COUNT),
                     "--rts-caps", str(dd.RTS_CAPABILITIES),
                     "--result", result],
                directory=tree)

        document = diagnosis_document()
        defaulted(document["baseline"], CLEAN_WT, f"{OUTSIDE}/baseline.json")
        defaulted(document["verification"], REPAIR_WT,
                  f"{OUTSIDE}/verify.json")
        shared_root = f"{OUTSIDE}/defaulted"
        document["baseline"]["result"] = result_document(
            runs=failing_runs(4), artifact_root=shared_root)
        document["verification"]["result"] = verification_result(
            artifact_root=shared_root)
        return document

    check_mutation(
        "the two batches hold distinct invocation directories",
        "creates a fresh one per invocation",
        "    if shared:", "    if False:", build)


def test_the_apparatus_inventory_is_what_refuses_a_harness_repair() -> None:
    check_mutation(
        "the measurement-apparatus inventory",
        "measurement apparatus",
        "    apparatus = [path for path in changed if path in "
        "HARNESS_MODULES]",
        "    apparatus = []",
        lambda: diagnosis_document(repair={
            "commit_sha": REPAIR_COMMIT, "base_sha": BASE_COMMIT,
            "changed_paths": ["tools/role_probe.py", "tools/probe_flake.py"]}))


def _collect() -> tuple:
    """This owner's tests, in source order, out of its own namespace.

    Derived rather than hand-listed, so a case added below joins the
    registry by existing: a hand-maintained roster is exactly the
    fourth list that could silently drop a case while the run still
    exited zero.

    `__module__` is checked because the names imported from the shared
    support module are in these globals too, and a helper that ever
    started with `test_` would otherwise be claimed by whichever owner
    imported it.
    """
    return tuple(value for name, value in globals().items()
                 if name.startswith("test_") and callable(value)
                 and getattr(value, "__module__", None) == __name__)


#: This owner's registry. The facade collects from exactly this, for
#: both the focused invocation and the aggregate one, so the two can
#: never disagree about what this owner declares.
TESTS = _collect()
