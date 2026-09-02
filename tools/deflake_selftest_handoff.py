#!/usr/bin/env python3
"""Diagnosis handoff cases (#1659), split out by #2093.

The 15 cases covering the retained handoff document: its naming beside
the result, the embedded measurement, the observed invocation data, the
targets, the configuration manifest, the installed census row the
acceptable-failure count comes from, which outcomes are eligible to
write one, writer failures, and the REAL writer against real
temporary-filesystem failures.

`Raiser` and `written_handoff` are this owner's alone -- no sibling
consumes either -- so they live here rather than in the shared support.

Not a gate of its own. Run through the aggregate:

  python3 tools/test_deflake.py --only handoff
"""
from __future__ import annotations

import hashlib
import json
import shutil
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import deflake  # type: ignore  # noqa: E402
import probe_census  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
import probe_protocol  # type: ignore  # noqa: E402
from deflake_selftest_support import (  # noqa: E402
    ARGV, COMMIT, CWD, OTHER_COMMIT, PROBE, FakeClaim, Recorder, Scratch,
    expect, installed_census, measurement, run)


# --------------------------------------------------------------------------
# The handoff (#1659)
# --------------------------------------------------------------------------
class Raiser:
    """A recording seam that fails the way one real failure fails."""

    def __init__(self, error) -> None:
        self.error = error

    def __call__(self, *args, **kwargs):
        raise self.error


def written_handoff(scratch: Scratch, **overrides):
    """Run to `recorded` and return `(result, handoff_document_or_None)`."""
    kept: dict = {}

    def save(path, document):
        kept[str(path)] = document
        return None

    settings = {"measure": Recorder(measurement(scratch)),
                "save_handoff": save}
    settings.update(overrides)
    result = run(scratch, **settings)
    if result.handoff_path is None:
        return result, None
    return result, kept.get(str(result.handoff_path))


def test_a_recorded_measurement_writes_its_handoff_beside_the_result() -> None:
    print("\n-- a recorded measurement writes a handoff beside its result")
    scratch = Scratch()
    try:
        result, document = written_handoff(scratch)
        expect(result.outcome == deflake.OUTCOME_RECORDED,
               f"({result.outcome})")
        expect(document is not None, "a handoff was written")
        expect(result.handoff_path is not None
               and Path(result.handoff_path).parent
               == Path(result.result_path).parent,
               f"beside the retained result ({result.handoff_path})")
        expect(str(result.handoff_path)
               == f"{result.result_path}{deflake.HANDOFF_SUFFIX}",
               f"named after it, so two results in one directory cannot "
               f"collide ({result.handoff_path})")
        expect(result.to_document()["handoff_document"]
               == str(result.handoff_path),
               "and the machine-readable outcome reports the path")
        expect(set(document) == {"schema", "probe", "acceptable_failures",
                                 "targets", "result", "invocation",
                                 "configuration", "artifacts"},
               f"the document has exactly the declared keys ({sorted(document)})")
        expect(document["schema"] == deflake.HANDOFF_SCHEMA,
               f"({document['schema']})")
    finally:
        scratch.cleanup()


def test_two_results_in_one_directory_get_two_handoffs() -> None:
    print("\n-- the handoff name is derived injectively from the result's")
    directory = Path("/tmp/one-directory")
    # `Path.stem` would map all three of these to `census`, so the later
    # measurement's handoff would overwrite the earlier one's.
    names = ["census.json", "census.txt", "census", "census.json.bak"]
    produced = [deflake.handoff_path_for(directory / name) for name in names]
    expect(len(set(produced)) == len(names),
           f"distinct results give distinct handoffs ({produced})")
    expect(all(path.parent == directory for path in produced),
           f"all beside their own result ({produced})")
    expect(all(path.name.endswith(deflake.HANDOFF_SUFFIX)
               for path in produced),
           f"and all recognisable as handoffs ({produced})")
    expect(deflake.handoff_path_for(directory / "census.json").name
           == f"census.json{deflake.HANDOFF_SUFFIX}",
           "derived from the WHOLE filename, not its stem")


def test_the_embedded_result_is_the_measurements_own_document() -> None:
    print("\n-- the result is embedded unchanged, not summarised")
    scratch = Scratch()
    try:
        real = measurement(scratch)
        result, document = written_handoff(scratch, measure=Recorder(real))
        expect(document["result"] == real.to_document(),
               "the embedded result is byte-for-byte the harness's own")
        expect(set(document["result"]) == set(real.to_document()),
               "and gains no field, because `probe-flake-result/v1` "
               "rejects additional properties")
        expect(document["probe"] == PROBE, f"({document['probe']})")
        expect(document["artifacts"] == real.retained_artifacts(),
               f"every retained path is named ({document['artifacts']})")
    finally:
        scratch.cleanup()


def test_the_invocation_records_what_the_process_observed() -> None:
    print("\n-- the invocation records argv, cwd, ports and both defaults")
    scratch = Scratch()
    try:
        real = measurement(scratch)
        result, document = written_handoff(scratch, measure=Recorder(real))
        invocation = document["invocation"]
        expect(set(invocation) == {"argv", "cwd", "retries", "ports",
                                   "timeout", "start_port"},
               f"exactly the declared keys ({sorted(invocation)})")
        expect(invocation["argv"] == ARGV,
               f"the observed argv, argv[0] included ({invocation['argv']})")
        expect(invocation["cwd"] == CWD, f"({invocation['cwd']})")
        expect(invocation["retries"] == 0,
               "the retry policy is 0 and this lab has no other")
        expect(invocation["ports"]
               == [run["port"] for run in real.to_document()["runs"]],
               f"the ordered BASE port of each completed run "
               f"({invocation['ports']})")
        expect(invocation["timeout"] == deflake.TIMEOUT
               and invocation["start_port"] == deflake.START_PORT,
               f"and the two settings passed to the adapter "
               f"({invocation['timeout']}, {invocation['start_port']})")
    finally:
        scratch.cleanup()


def test_the_adapter_is_told_the_timeout_and_starting_port() -> None:
    print("\n-- both are passed EXPLICITLY, like the run and capability counts")
    scratch = Scratch()
    try:
        recorder = Recorder(measurement(scratch))
        run(scratch, measure=recorder, save_handoff=lambda p, d: None)
        expect(len(recorder.calls) == 1, f"({recorder.calls})")
        call = recorder.calls[0]
        expect(call["timeout"] == deflake.TIMEOUT,
               f"the timeout is supplied, not defaulted ({call['timeout']})")
        expect(call["start_port"] == deflake.START_PORT,
               f"and so is the starting port ({call['start_port']})")
        expect(deflake.TIMEOUT == probe_flake.DEFAULT_TIMEOUT
               and deflake.START_PORT == probe_flake.PORT_MIN,
               "each equal to the harness's own value, supplied rather "
               "than relied on")
    finally:
        scratch.cleanup()


def test_the_targets_are_the_non_pass_identifiers_in_descriptor_order() -> None:
    print("\n-- targets: once each, FAIL or MISSING, in the declared order")
    scratch = Scratch()
    try:
        # Declared gamma, alpha, beta — deliberately not alphabetical, so
        # sorting the identifiers and reading the descriptor give
        # DIFFERENT answers and only one of them is the contract.
        descriptor = probe_protocol.build_descriptor(
            PROBE, [("gamma", "first"), ("alpha", "second"),
                    ("beta", "third")])
        invocation = scratch.artifacts / "multi"
        invocation.mkdir(parents=True, exist_ok=True)
        real = probe_flake.Measurement(
            PROBE, descriptor, deflake.CENSUS_RUN_COUNT,
            deflake.RTS_CAPABILITIES, scratch.artifacts, invocation)
        real.commit_sha = COMMIT
        real.timestamp = "2026-08-21T12:00:00Z"
        # gamma fails twice and beta goes MISSING once; alpha never
        # fails. Declared order gives [gamma, beta]; sorting would give
        # [beta, gamma], and repeating per run would give gamma twice.
        maps = [{"gamma": probe_protocol.FAIL, "alpha": probe_protocol.PASS,
                 "beta": probe_protocol.PASS},
                {"gamma": probe_protocol.MISSING, "alpha": probe_protocol.PASS,
                 "beta": probe_protocol.MISSING},
                {"gamma": probe_protocol.FAIL, "alpha": probe_protocol.PASS,
                 "beta": probe_protocol.PASS}]
        maps += [{"gamma": probe_protocol.PASS, "alpha": probe_protocol.PASS,
                  "beta": probe_protocol.PASS}] * 7
        for index, checks in enumerate(maps, 1):
            outcome = (probe_flake.RUN_PASS
                       if all(v == probe_protocol.PASS for v in checks.values())
                       else probe_flake.RUN_FAIL)
            run_dir = invocation / f"run-{index:03d}"
            run_dir.mkdir(parents=True, exist_ok=True)
            real.runs.append(probe_flake.RunRecord(
                index, 9100 + index, outcome, 1.5, checks,
                run_dir if outcome != probe_flake.RUN_PASS else None))
        _result, document = written_handoff(scratch, measure=Recorder(real))
        expect(document["targets"] == ["gamma", "beta"],
               f"once each, in the DECLARED order — sorting would answer "
               f"['beta', 'gamma'] ({document['targets']})")
        expect(deflake.handoff_targets(real.to_document()) == ["gamma", "beta"],
               "and the helper answers the same alone")
        expect(deflake.handoff_targets(measurement(scratch).to_document()) == [],
               "a batch that passed everywhere targets nothing")
    finally:
        scratch.cleanup()


def test_the_configuration_manifest_records_contents_and_absence() -> None:
    print("\n-- the config manifest: every family member, or an empty list")
    root = Path(tempfile.mkdtemp(prefix="test_deflake_config_"))
    try:
        (root / "config").mkdir()
        expect(deflake.configuration_manifest(root) == [],
               "an absent family is an EMPTY LIST, stated positively")

        # Created in reverse-alphabetical order, so directory order and
        # sorted order are different answers.
        for name in ("video.local.yaml", "save.local.yaml",
                     "notifications.local.yaml", "keybinds.local.yaml"):
            (root / "config" / name).write_text(f"{name}: true\n")
        (root / "config" / "ignored.yaml").write_text("not in the family\n")
        (root / "config" / "nested").mkdir()
        entries = deflake.configuration_manifest(root)
        paths = [entry["path"] for entry in entries]
        expect(paths == ["config/keybinds.local.yaml",
                         "config/notifications.local.yaml",
                         "config/save.local.yaml",
                         "config/video.local.yaml"],
               f"only the gitignored family, SORTED by path ({paths})")
        expect(paths == sorted(paths), f"explicitly ({paths})")
        expect(all(set(entry) == {"path", "sha256"} for entry in entries),
               f"exactly two keys per entry ({entries})")
        digest = hashlib.sha256(
            (root / "config" / "keybinds.local.yaml").read_bytes()).hexdigest()
        expect(entries[0]["sha256"] == digest and digest == digest.lower(),
               "the lowercase SHA-256 of the bytes actually read")
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_the_configuration_is_read_under_the_hold_before_the_runs() -> None:
    print("\n-- the manifest describes what the runs read, not what followed")
    scratch = Scratch()
    try:
        order: list = []
        recorder = Recorder(measurement(scratch))

        def measure(*args, **kwargs):
            order.append("measure")
            return recorder(*args, **kwargs)

        def read_configuration(root):
            order.append("configuration")
            return [{"path": "config/save.local.yaml", "sha256": "a" * 64}]

        _result, document = written_handoff(
            scratch, measure=measure, read_configuration=read_configuration)
        expect(order == ["configuration", "measure"],
               f"captured before the first engine ({order})")
        expect(document["configuration"]
               == [{"path": "config/save.local.yaml", "sha256": "a" * 64}],
               f"and carried verbatim ({document['configuration']})")
    finally:
        scratch.cleanup()


def test_a_configuration_that_cannot_be_read_is_a_managed_failure() -> None:
    print("\n-- capturing the configuration OPENS files, and that can fail")
    scratch = Scratch()
    try:
        recorder = Recorder(measurement(scratch))
        claim = FakeClaim()
        attempts: list = []

        def unreadable(root):
            raise PermissionError(13, "Permission denied",
                                  "config/save.local.yaml")

        result = run(scratch, measure=recorder,
                     acquire_claim=lambda probe, **kw: claim,
                     read_configuration=unreadable,
                     save_handoff=lambda path, document: attempts.append(path))
        expect(result.outcome == deflake.OUTCOME_MANAGED_ERROR,
               f"it is a managed outcome, not a traceback ({result.outcome})")
        expect(result.exit_code != 0, f"and nonzero ({result.exit_code})")
        expect("could not be captured" in (result.detail or ""),
               f"naming what failed ({result.detail})")
        expect(recorder.calls == [],
               f"no engine was started ({recorder.calls})")
        expect(attempts == [] and result.handoff_path is None
               and result.to_document()["handoff_document"] is None,
               "and there is no handoff, because nothing was measured")
        expect(claim.released and result.ownership == deflake.OWNERSHIP_NONE,
               f"the claim went back ({result.ownership})")

        # And when the release ALSO fails, the retained ownership is the
        # result rather than a footnote on it — the same rule every
        # other pre-measurement failure follows.
        held = FakeClaim(release_error="the claim file is unwritable")
        result = run(scratch, measure=Recorder(measurement(scratch)),
                     acquire_claim=lambda probe, **kw: held,
                     read_configuration=unreadable)
        expect(result.outcome == deflake.OUTCOME_MANAGED_ERROR
               and result.ownership == deflake.OWNERSHIP_CLAIM_HELD,
               f"({result.outcome}, {result.ownership})")

        # The shipped reader OPENS each file and swallows nothing, which
        # is what makes the injected one a fair stand-in. Asserted by
        # reading the source rather than by denying permission: CI runs
        # as root, where a mode of 0 stops nothing, and a test that
        # quietly skipped there would be coverage only on a laptop.
        source = Path(deflake.__file__).read_text(encoding="utf-8")
        body = source[source.index("def configuration_manifest"):
                      source.index("def handoff_targets")]
        expect("open(path, \"rb\")" in body,
               "the reader opens each file, so it can fail like one")
        expect("except" not in body,
               f"and catches nothing, so the caller sees the OSError")
    finally:
        scratch.cleanup()


def test_the_acceptable_failure_count_comes_from_the_installed_row() -> None:
    print("\n-- X comes from the row the transaction wrote, not a reread")
    scratch = Scratch()
    try:
        _result, document = written_handoff(
            scratch,
            record_result=lambda *a, **kw: (PROBE,
                                            installed_census(acceptable=3)))
        expect(document["acceptable_failures"] == 3,
               f"({document['acceptable_failures']})")

        # The lock is released when the recorder returns, so a row that
        # names no X is a refusal rather than a reread.
        result = run(scratch, measure=Recorder(measurement(scratch)),
                     record_result=lambda *a, **kw: (PROBE, {"probes": []}))
        expect(result.outcome == deflake.OUTCOME_MANAGED_ERROR,
               f"a row with no acceptable-failure count refuses "
               f"({result.outcome})")
        expect(result.handoff_path is None
               and result.to_document()["handoff_document"] is None,
               "and writes no handoff")
    finally:
        scratch.cleanup()


def test_a_recorder_that_answers_the_wrong_shape_is_refused() -> None:
    print("\n-- the recording seam answers (probe, installed census)")
    scratch = Scratch()
    try:
        for label, answer in (("only the probe key", PROBE),
                              ("a one-element tuple", (PROBE,)),
                              ("a probe and a string", (PROBE, "census")),
                              ("nothing at all", None)):
            result = run(scratch, measure=Recorder(measurement(scratch)),
                         record_result=lambda *a, _v=answer, **kw: _v)
            expect(result.outcome == deflake.OUTCOME_MANAGED_ERROR,
                   f"a seam answering {label} is refused ({result.outcome})")
            expect("not the probe and the census document"
                   in (result.detail or ""),
                   f"by name, rather than raising from inside a committed "
                   f"transaction ({result.detail})")
            expect(result.handoff_path is None, "and no handoff is written")
    finally:
        scratch.cleanup()


def test_only_the_recorded_outcome_has_a_handoff() -> None:
    print("\n-- every other post-measurement outcome writes none")
    scratch = Scratch()
    try:
        attempts: list = []

        def save(path, document):
            attempts.append(str(path))
            return None

        cases = {
            deflake.OUTCOME_HARNESS_ERROR: dict(
                measure=Recorder(measurement(scratch, harness_error=True))),
            deflake.OUTCOME_COMMIT_CHANGED: dict(
                measure=Recorder(measurement(scratch)),
                head_commit=lambda: OTHER_COMMIT),
            deflake.OUTCOME_RECORD_FAILED: dict(
                measure=Recorder(measurement(scratch)),
                record_result=Raiser(probe_census.CensusError("no"))),
            deflake.OUTCOME_RECORD_INDETERMINATE: dict(
                measure=Recorder(measurement(scratch)),
                record_result=Raiser(
                    probe_census.CensusDurabilityUnconfirmed(
                    "the directory fsync failed", target="census",
                    error=OSError("fsync")))),
            deflake.OUTCOME_RECORDED_RELEASE_FAILED: dict(
                measure=Recorder(measurement(scratch)),
                acquire_claim=lambda probe, **kw: FakeClaim(
                    probe, release_error="the claim file is unwritable")),
        }
        for expected, overrides in cases.items():
            attempts.clear()
            result = run(scratch, save_handoff=save, **overrides)
            expect(result.outcome == expected,
                   f"{expected} reached ({result.outcome})")
            expect(result.handoff_path is None,
                   f"{expected} reports no handoff ({result.handoff_path})")
            expect(result.to_document()["handoff_document"] is None,
                   f"{expected} nulls the field rather than omitting it")
            expect(attempts == [],
                   f"{expected} did not even attempt a write ({attempts})")
    finally:
        scratch.cleanup()


def test_a_recorded_measurement_with_nothing_retained_cannot_hand_off() -> None:
    print("\n-- `recorded` does not guarantee a retained result to sit beside")
    scratch = Scratch()
    try:
        real = measurement(scratch)
        real.invocation_dir = None
        # An ancestor that is a regular FILE, so `mkdir(parents=True)`
        # raises `NotADirectoryError` for every user. A path under a
        # nonexistent root would not do: CI runs as root in a container,
        # where `/nonexistent/dir/...` is perfectly creatable, and the
        # measurement would be retained after all.
        blocker = scratch.root / "a-regular-file"
        blocker.write_text("not a directory\n", encoding="utf-8")
        result = run(scratch, measure=Recorder(real),
                     result_path=str(blocker / "sub" / "r.json"))
        expect(result.outcome == deflake.OUTCOME_MANAGED_ERROR,
               f"({result.outcome})")
        expect(result.result_path is None,
               f"nothing was retained ({result.result_path})")
        expect(result.handoff_path is None
               and result.to_document()["handoff_document"] is None,
               "so there is no handoff")
        expect("WAS recorded in the census" in (result.detail or ""),
               f"and the detail says the census update stands ({result.detail})")
        expect(result.ownership == deflake.OWNERSHIP_NONE,
               f"the claim was already released ({result.ownership})")
    finally:
        scratch.cleanup()


def test_a_handoff_that_cannot_be_written_is_a_managed_error() -> None:
    print("\n-- the census update stands, and is neither retried nor rolled back")
    scratch = Scratch()
    try:
        result = run(scratch, measure=Recorder(measurement(scratch)),
                     record_result=probe_census.record_result_installed,
                     save_handoff=lambda path, document: "the disk is full")
        expect(result.outcome == deflake.OUTCOME_MANAGED_ERROR,
               f"({result.outcome})")
        expect(result.exit_code != 0, f"and nonzero ({result.exit_code})")
        expect(result.handoff_path is None
               and result.to_document()["handoff_document"] is None,
               "the handoff field is null")
        expect("the disk is full" in (result.detail or "")
               and "WAS recorded in the census" in (result.detail or ""),
               f"both facts are reported ({result.detail})")
        expect(result.ownership == deflake.OWNERSHIP_NONE,
               f"and nothing is still owned ({result.ownership})")
        cohort = scratch.census_of()
        expect(cohort.get("current") is not None,
               "the committed census update was left exactly as it was")
    finally:
        scratch.cleanup()


def test_the_real_writer_produces_a_readable_document() -> None:
    print("\n-- the shipped writer, against a real directory")
    scratch = Scratch()
    try:
        target = scratch.root / "beside" / "result-handoff.json"
        document = {"schema": deflake.HANDOFF_SCHEMA, "probe": PROBE}
        expect(deflake.write_handoff(target, document) is None,
               "it writes, creating the directory")
        expect(json.loads(target.read_text(encoding="utf-8")) == document,
               "and the bytes read back as the document")
        problem = deflake.write_handoff(scratch.root / "beside", document)
        expect(problem is not None and "could not write" in problem,
               f"an unwritable target is a reported problem, not a "
               f"traceback ({problem})")
        # Here the staging file WAS created and the rename is what
        # failed, so this is the case that proves it gets cleaned up.
        expect(not (scratch.root / "beside.partial").exists(),
               f"and the staging file it created is removed "
               f"({sorted(path.name for path in scratch.root.iterdir())})")

        # A failed write leaves NO handoff: only `recorded` may leave
        # one, and a partial file beside the result would tell a later
        # consumer it had a complete measurement when it does not.
        unserialisable = scratch.root / "beside" / "bad-handoff.json"
        unserialisable.parent.mkdir(parents=True, exist_ok=True)
        unserialisable.write_text("PRIOR CONTENT\n", encoding="utf-8")
        problem = deflake.write_handoff(unserialisable, {"runs": {1, 2}})
        expect(problem is not None and "could not serialize" in problem,
               f"a document json cannot encode is reported ({problem})")
        expect(unserialisable.read_text(encoding="utf-8") == "PRIOR CONTENT\n",
               "and the target was never even opened, let alone truncated")

        # A write whose own I/O fails, with an existing handoff in place:
        # an ancestor that is a regular FILE refuses `mkdir` for every
        # user, so this holds in a root CI container too — a chmod would
        # not, which is exactly how the first version of this suite
        # passed locally and failed in CI.
        blocker = scratch.root / "blocking-file"
        blocker.write_text("not a directory\n", encoding="utf-8")
        problem = deflake.write_handoff(blocker / "deep" / "x-handoff.json",
                                        document)
        expect(problem is not None and "could not write" in problem,
               f"an uncreatable target is reported ({problem})")
        expect(blocker.read_text(encoding="utf-8") == "not a directory\n",
               "and nothing on the way to it was disturbed")
    finally:
        scratch.cleanup()


#: This owner's inventory, in the order the aggregate has always run it.
CASES = (
    test_a_recorded_measurement_writes_its_handoff_beside_the_result,
    test_two_results_in_one_directory_get_two_handoffs,
    test_the_embedded_result_is_the_measurements_own_document,
    test_the_invocation_records_what_the_process_observed,
    test_the_adapter_is_told_the_timeout_and_starting_port,
    test_the_targets_are_the_non_pass_identifiers_in_descriptor_order,
    test_the_configuration_manifest_records_contents_and_absence,
    test_the_configuration_is_read_under_the_hold_before_the_runs,
    test_a_configuration_that_cannot_be_read_is_a_managed_failure,
    test_the_acceptable_failure_count_comes_from_the_installed_row,
    test_a_recorder_that_answers_the_wrong_shape_is_refused,
    test_only_the_recorded_outcome_has_a_handoff,
    test_a_recorded_measurement_with_nothing_retained_cannot_hand_off,
    test_a_handoff_that_cannot_be_written_is_a_managed_error,
    test_the_real_writer_produces_a_readable_document,
)
