#!/usr/bin/env python3
"""The save-compat self-test's structural-comparison owner (issue #2274).

`compare_session_snapshots` is the fourth operation
`tools/save_compat_audit_codec.py` owns, and the only one whose non-`OK`
outcomes are ANSWERS rather than malfunctions: "these saves differ" and
"one of them will not decode" both describe the fixtures, while "the
helper could not be run" describes the toolchain. Three outcomes plus an
error, where the other three operations have a bare (ok, tail) pair --
so the classification is the thing to cover, in both directions.

Its one consumer is covered here too rather than in a module of its own.
`tools/persistence_snapshot.py`'s whole job is to turn this operation's
report into a probe's failure text, and the interesting property of that
translation -- WHICH two saves the canonical-summary diagnostic compares
-- can only be exercised against real decoded envelopes, which is this
module's fixture corpus. Splitting them would mean a second module
reading the same corpus for the same reason.

The `gen3` case is the one to read first. The pre-#2274 diagnostic
summarized `paths[:2]` unconditionally, so a run whose first divergence
appeared in the third or fourth generation reported a mismatch and then a
structural diff showing NO difference at all -- the two files it compared
really were identical. `compare`'s report names the divergent path, and
`diff_pair` uses it.

Every member here needs the built helper, exactly as the codec owner's do
(`cabal build all` produces it), and reaches the real codec against the
tracked corpus -- no engine, no GPU, no worldgen. Nothing is written
inside `test-headless/data/save-compat/`: the mismatch corpora are
COPIES, assembled in a temporary directory, because the interesting
inputs are "four paths, the third one different" and no tracked pair has
that shape.
"""
from __future__ import annotations

import json
import os
import shutil
import stat
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import persistence_snapshot  # noqa: E402
import save_compat_audit_codec as codec  # noqa: E402
import save_compat_audit_common as common  # noqa: E402

from selftestlib import expect  # noqa: E402


def _compare(paths: list[Path]) -> tuple[str, dict | None, str]:
    """`compare_session_snapshots` over `paths`, as this module calls it."""
    return codec.compare_session_snapshots(paths)


def _pairwise(reference: Path, other: Path) -> tuple[str, dict | None]:
    outcome, report, _detail = _compare([reference, other])
    return outcome, report


def _two_distinct_fixtures() -> tuple[Path, Path]:
    """Two tracked fixtures that really decode, and really decode
    DIFFERENTLY.

    Derived from the corpus rather than named, and VERIFIED rather than
    assumed. Two of the tracked `*.bin` files are Lua-component payloads
    rather than whole envelopes and do not decode at all, and a renamed
    or retired baseline could otherwise leave this module comparing one
    fixture with itself -- either of which would make every mismatch
    member below vacuously pass while still going green.
    """
    candidates = sorted(common.FIXTURE_DATA_DIR.glob("*.bin"))
    reference = None
    for candidate in candidates:
        if reference is None:
            outcome, _ = _pairwise(candidate, candidate)
            if outcome == codec.COMPARE_OK:
                reference = candidate
            continue
        outcome, _ = _pairwise(reference, candidate)
        if outcome == codec.COMPARE_MISMATCH:
            return reference, candidate
    raise AssertionError(
        f"no two fixtures under {common.FIXTURE_DATA_DIR} both decode and "
        f"decode differently; every mismatch member in this owner would be "
        f"vacuous (scanned {len(candidates)})")


def _divergent_on(reference: Path, half: str, other_half: str) -> Path | None:
    """A fixture differing from `reference` on `half` but not `other_half`.

    Used to prove the two halves are computed and reported
    independently rather than collapsed into one "differs" answer -- and,
    in the `luaComponentDiffers` direction, that the Lua payload
    comparison exists at all. A comparison that silently dropped one half
    would still answer "mismatch" for almost every pair in this corpus,
    because almost every pair differs on both.

    Returns None when the corpus holds no such fixture, which its caller
    reports as a failure rather than skipping silently.
    """
    for candidate in sorted(common.FIXTURE_DATA_DIR.glob("*.bin")):
        outcome, report = _pairwise(reference, candidate)
        if outcome != codec.COMPARE_MISMATCH or not isinstance(report, dict):
            continue
        if report.get(half) and not report.get(other_half):
            return candidate
    return None


def _divergent_on_both(reference: Path) -> Path | None:
    """A fixture differing from `reference` on BOTH halves at once."""
    for candidate in sorted(common.FIXTURE_DATA_DIR.glob("*.bin")):
        outcome, report = _pairwise(reference, candidate)
        if outcome != codec.COMPARE_MISMATCH or not isinstance(report, dict):
            continue
        if report.get("snapshotDiffers") and report.get(
                "luaComponentDiffers"):
            return candidate
    return None


def _generations(tmp: Path, reference: Path, divergent: Path,
                 divergent_index: int) -> list[Path]:
    """Four "generations": copies of `reference`, one of them different.

    Named gen1..gen4 so a failure reads the way the probe's own output
    does. `divergent_index` is 0-based; passing one past the end yields
    four identical generations.
    """
    paths = []
    for index in range(4):
        source = divergent if index == divergent_index else reference
        target = tmp / f"gen{index + 1}.synworld"
        shutil.copyfile(source, target)
        paths.append(target)
    return paths


def _fake_helper(tmp: Path, body: str) -> Path:
    """A stand-in helper running `body`, for the outcomes a real one cannot
    produce on demand."""
    script = tmp / "fake_codec_helper"
    script.write_text(f"#!/bin/sh\n{body}\n", encoding="utf-8")
    script.chmod(script.stat().st_mode | stat.S_IXUSR)
    return script


def _with_helper(path: Path):
    """Point the bridge's pre-resolved handoff at `path` for one block."""
    class _Scope:
        def __enter__(self) -> None:
            self.previous = os.environ.get(codec.ENV_CODEC_EXE)
            os.environ[codec.ENV_CODEC_EXE] = str(path)

        def __exit__(self, *_exc) -> None:
            if self.previous is None:
                os.environ.pop(codec.ENV_CODEC_EXE, None)
            else:
                os.environ[codec.ENV_CODEC_EXE] = self.previous
    return _Scope()


def test_identical_generations_compare_ok() -> None:
    print("\n-- four identical generations decode to one COMPARE_OK")
    reference, divergent = _two_distinct_fixtures()
    tmp = Path(tempfile.mkdtemp(prefix="compare_ok_"))
    try:
        paths = _generations(tmp, reference, divergent, divergent_index=9)
        outcome, report, detail = codec.compare_session_snapshots(paths)
        expect(outcome == codec.COMPARE_OK,
               f"the outcome is ok (got {outcome!r}: {detail!r})")
        expect(detail == "", f"with no diagnostic (got {detail!r})")
        expect(isinstance(report, dict)
               and report.get("reference") == str(paths[0]),
               f"and a report naming the first path as the reference "
               f"(got {report!r})")
        expect(isinstance(report, dict)
               and report.get("snapshotDiffers") == []
               and report.get("luaComponentDiffers") == [],
               f"with both difference lists empty (got {report!r})")
        ok, probe_detail = persistence_snapshot.compare_session_files(paths)
        expect(ok and probe_detail == "",
               f"and the probe-facing wrapper agrees (got {ok}, "
               f"{probe_detail!r})")
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def test_a_mismatch_names_the_generation_that_first_diverged() -> None:
    print("\n-- a divergence in gen3 is reported AS gen3, not as gen2")
    reference, divergent = _two_distinct_fixtures()
    tmp = Path(tempfile.mkdtemp(prefix="compare_gen3_"))
    try:
        paths = _generations(tmp, reference, divergent, divergent_index=2)
        outcome, report, detail = codec.compare_session_snapshots(paths)
        expect(outcome == codec.COMPARE_MISMATCH,
               f"the outcome is a mismatch (got {outcome!r}: {detail!r})")
        expect("COMPARE_MISMATCH" in detail,
               f"and the human line carries the helper's own marker "
               f"(got {detail!r})")
        expect(isinstance(report, dict)
               and report.get("snapshotDiffers") == [str(paths[2])],
               f"the report names gen3 and only gen3 (got {report!r})")
        # The property the pre-#2274 diagnostic got wrong: it summarized
        # paths[:2], which here are two byte-identical copies.
        pair = persistence_snapshot.diff_pair(paths, report)
        expect(pair == [paths[0], paths[2]],
               f"so the diagnostic diffs gen1 against gen3 (got {pair})")
        ok, probe_detail = persistence_snapshot.compare_session_files(paths)
        expect(not ok, f"the probe-facing wrapper fails (got {ok})")
        expect("first structural difference" in probe_detail,
               f"and carries a REAL structural difference rather than a "
               f"mismatch with nothing behind it (got {probe_detail!r})")
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def test_a_gen4_divergence_is_reported_too() -> None:
    print("\n-- and so is one that appears only in the LAST generation")
    reference, divergent = _two_distinct_fixtures()
    tmp = Path(tempfile.mkdtemp(prefix="compare_gen4_"))
    try:
        paths = _generations(tmp, reference, divergent, divergent_index=3)
        outcome, report, _detail = codec.compare_session_snapshots(paths)
        expect(outcome == codec.COMPARE_MISMATCH,
               f"the outcome is a mismatch (got {outcome!r})")
        expect(isinstance(report, dict)
               and report.get("snapshotDiffers") == [str(paths[3])],
               f"naming gen4 (got {report!r})")
        expect(persistence_snapshot.diff_pair(paths, report)
               == [paths[0], paths[3]],
               f"and the diagnostic diffs gen1 against gen4 (got "
               f"{persistence_snapshot.diff_pair(paths, report)})")
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def test_an_undecodable_generation_is_a_decode_failure_not_a_mismatch() -> None:
    print("\n-- a save that will not decode is DECODE_FAILED, named")
    reference, divergent = _two_distinct_fixtures()
    tmp = Path(tempfile.mkdtemp(prefix="compare_decode_"))
    try:
        paths = _generations(tmp, reference, divergent, divergent_index=9)
        paths[2].write_bytes(b"not an envelope at all")
        outcome, report, detail = codec.compare_session_snapshots(paths)
        expect(outcome == codec.COMPARE_DECODE_FAILED,
               f"the outcome is a decode failure, which is a different "
               f"diagnosis from 'these differ' (got {outcome!r}: {detail!r})")
        expect("DECODE_FAILED" in detail,
               f"the human line carries the helper's marker (got {detail!r})")
        errors = (report or {}).get("decodeErrors")
        expect(isinstance(errors, list) and len(errors) == 1
               and errors[0].get("path") == str(paths[2]),
               f"the report names the offending fixture (got {report!r})")
        expect(isinstance(errors, list) and errors
               and str(errors[0].get("error") or "").strip(),
               f"and the production codec's own error text beside it "
               f"(got {report!r})")
        ok, probe_detail = persistence_snapshot.compare_session_files(paths)
        expect(not ok and "DECODE_FAILED" in probe_detail,
               f"the probe-facing wrapper propagates it (got {ok}, "
               f"{probe_detail!r})")
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def test_a_helper_that_cannot_be_run_is_not_a_verdict_about_the_saves() -> None:
    print("\n-- a decoder that will not launch is COMPARE_ERROR, never a "
          "mismatch")
    reference, divergent = _two_distinct_fixtures()
    tmp = Path(tempfile.mkdtemp(prefix="compare_error_"))
    try:
        paths = _generations(tmp, reference, divergent, divergent_index=9)
        missing = tmp / "never-built"
        with _with_helper(missing):
            outcome, report, detail = codec.compare_session_snapshots(paths)
        expect(outcome == codec.COMPARE_ERROR,
               f"an exported path naming no file is an error (got "
               f"{outcome!r})")
        expect(report is None, f"with no report to read (got {report!r})")
        expect(str(missing) in detail,
               f"naming the path it could not use (got {detail!r})")

        # A helper that RUNS but decides nothing: exiting without any of
        # the three markers is this bridge's failure, not the corpus's.
        silent = _fake_helper(tmp, "exit 3")
        with _with_helper(silent):
            outcome, report, detail = codec.compare_session_snapshots(paths)
        expect(outcome == codec.COMPARE_ERROR,
               f"and so is an exit with no marker at all (got {outcome!r})")
        expect("COMPARE_OK" in detail and "3" in detail,
               f"naming what it looked for and the status it got "
               f"(got {detail!r})")
        ok, probe_detail = persistence_snapshot.compare_session_files(paths)
        expect(ok is not None, "the wrapper still answers rather than raising")
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def test_a_marker_inside_a_fixture_path_is_not_read_as_an_outcome() -> None:
    print("\n-- a save whose own NAME contains a marker cannot forge a "
          "verdict")
    reference, divergent = _two_distinct_fixtures()
    tmp = Path(tempfile.mkdtemp(prefix="compare_forged_"))
    try:
        paths = _generations(tmp, reference, divergent, divergent_index=9)
        # An ordinary path for a probe to hand this -- and one that puts
        # the literal text `COMPARE_OK` inside the helper's own
        # `DECODE_FAILED: [("...",...)]` line. A substring scan over the
        # combined output reads that as success and lets
        # `compare_session_files` pass on saves it never compared.
        forged = tmp / "gen2-COMPARE_OK.synworld"
        paths[1].rename(forged)
        forged.write_bytes(b"not an envelope at all")
        paths[1] = forged
        outcome, report, detail = codec.compare_session_snapshots(paths)
        expect("COMPARE_OK" in detail,
               f"the helper's line really does carry the marker text "
               f"(got {detail!r})")
        expect(outcome == codec.COMPARE_DECODE_FAILED,
               f"and the outcome is still the decode failure it really is "
               f"(got {outcome!r})")
        expect((report or {}).get("outcome") == codec.COMPARE_DECODE_FAILED,
               f"corroborated by the report (got {report!r})")
        ok, probe_detail = persistence_snapshot.compare_session_files(paths)
        expect(not ok,
               f"so the probe-facing wrapper FAILS rather than passing on "
               f"saves it never compared (got {ok}, {probe_detail!r})")
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def test_an_answer_this_bridge_cannot_corroborate_is_an_error() -> None:
    print("\n-- marker, exit status and report must agree, or the outcome "
          "is COMPARE_ERROR")
    reference, divergent = _two_distinct_fixtures()
    tmp = Path(tempfile.mkdtemp(prefix="compare_incoherent_"))
    try:
        paths = _generations(tmp, reference, divergent, divergent_index=2)
        ok_line = "COMPARE_OK"
        mismatch_line = ('COMPARE_MISMATCH: snapshot-differs=[] '
                         'lua-component-differs=[]')
        # argv is `compare --output <report> <gen1> ...`, so the report
        # path the fake helper must write to is $3.
        report_arg = '"$3"'
        for label, body, needle in (
            # Marker without a report at all: the verdict cannot be
            # corroborated, so it is not believed.
            ("a marker with no report",
             f'echo "{mismatch_line}"\nexit 1', "no readable report"),
            # Marker contradicted by the exit status, in both directions.
            ("a success marker with a nonzero status",
             f'echo "{ok_line}"\n'
             f'printf \'{{"outcome":"ok"}}\' > {report_arg}\nexit 1',
             "the protocol assigns that line exit status 0"),
            ("a mismatch marker with a zero status",
             f'echo "{mismatch_line}"\n'
             f'printf \'{{"outcome":"mismatch"}}\' > {report_arg}\nexit 0',
             "the protocol assigns that line exit status 1"),
            # Marker contradicted by the report's own outcome field.
            ("a marker the report disagrees with",
             f'echo "{mismatch_line}"\n'
             f'printf \'{{"outcome":"ok"}}\' > {report_arg}\nexit 1',
             "report names outcome"),
            # Two protocol lines in one run: which one is the answer?
            ("two conflicting protocol lines",
             f'echo "{ok_line}"\necho "{mismatch_line}"\n'
             f'printf \'{{"outcome":"ok"}}\' > {report_arg}\nexit 1',
             "conflicting outcomes"),
            # ... and two that AGREE. "They said the same thing twice"
            # is not a reason to believe a helper that emits one line
            # per run: agreement is what a marker-shaped path exploits,
            # so the shape is refused rather than reconciled.
            ("the same protocol line twice",
             f'echo "{mismatch_line}"\necho "{mismatch_line}"\n'
             f'printf \'{{"outcome":"mismatch"}}\' > {report_arg}\nexit 1',
             "2 protocol lines"),
            # A coherent mismatch response that exits 2 rather than the
            # 1 the protocol assigns it: `exitFailure` is 1, so this is
            # a helper that died on the way out after deciding.
            ("a mismatch marker with an unexpected nonzero status",
             f'echo "{mismatch_line}"\n'
             f'printf \'{{"outcome":"mismatch"}}\' > {report_arg}\nexit 2',
             "the protocol assigns that line exit status 1"),
            ("a decode failure with an unexpected nonzero status",
             f'echo "DECODE_FAILED: [(\\"/x\\",\\"BadMagic\\")]"\n'
             f'printf \'{{"outcome":"decode_failed"}}\' > {report_arg}\n'
             f'exit 3',
             "the protocol assigns that line exit status 1"),
            # A report that is not the promised object.
            ("a report that is not an object",
             f'echo "{mismatch_line}"\n'
             f'printf \'[1,2,3]\' > {report_arg}\nexit 1',
             "not the object the protocol promises"),
        ):
            helper = _fake_helper(tmp, body)
            with _with_helper(helper):
                outcome, report, detail = codec.compare_session_snapshots(
                    paths)
            expect(outcome == codec.COMPARE_ERROR,
                   f"{label} is an error about the toolchain, not a verdict "
                   f"about the saves (got {outcome!r}: {detail!r})")
            expect(report is None,
                   f"{label} yields no report to act on (got {report!r})")
            expect(needle in detail,
                   f"{label} says WHICH signal disagreed (wanted {needle!r}, "
                   f"got {detail!r})")
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def test_the_diagnostic_pair_falls_back_without_a_usable_report() -> None:
    print("\n-- diff_pair degrades to the first two paths rather than "
          "raising")
    tmp = Path(tempfile.mkdtemp(prefix="compare_pair_"))
    try:
        paths = [tmp / f"gen{i}.synworld" for i in range(1, 5)]
        for path in paths:
            path.write_bytes(b"")
        # A pure unit of the consumer's own selection, so the fallback is
        # exercised without asking the helper to malfunction: `None`, a
        # non-object, a report naming nothing divergent, and one naming a
        # path that is not in this run at all.
        for label, report in (("no report", None),
                              ("a non-object report", ["nope"]),
                              ("nothing divergent",
                               {"reference": str(paths[0]),
                                "snapshotDiffers": [],
                                "luaComponentDiffers": []}),
                              ("a foreign path",
                               {"reference": str(paths[0]),
                                "snapshotDiffers": ["/elsewhere/gen9"],
                                "luaComponentDiffers": []})):
            expect(persistence_snapshot.diff_pair(paths, report) == paths[:2],
                   f"{label} falls back to the first two paths (got "
                   f"{persistence_snapshot.diff_pair(paths, report)})")
        expect(persistence_snapshot.diff_pair(
                   paths, {"reference": str(paths[0]),
                           "snapshotDiffers": [str(paths[3])],
                           "luaComponentDiffers": [str(paths[2])]})
               == [paths[0], paths[2]],
               "and the EARLIER of two divergent paths wins, whichever half "
               "names it")
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def test_fewer_than_two_paths_never_reaches_the_helper() -> None:
    print("\n-- one save is trivially equal to itself, with no subprocess")
    reference, _divergent = _two_distinct_fixtures()
    tmp = Path(tempfile.mkdtemp(prefix="compare_single_"))
    try:
        # A helper that would FAIL loudly if it were reached at all.
        refusing = _fake_helper(tmp, "exit 42")
        with _with_helper(refusing):
            outcome, report, detail = codec.compare_session_snapshots(
                [reference])
            expect(outcome == codec.COMPARE_OK,
                   f"a single path is ok (got {outcome!r}: {detail!r})")
            expect(isinstance(report, dict)
                   and report.get("snapshotDiffers") == [],
                   f"with an empty difference list (got {report!r})")
            outcome, _report, _detail = codec.compare_session_snapshots([])
            expect(outcome == codec.COMPARE_OK,
                   f"and so is none (got {outcome!r})")
            ok, probe_detail = persistence_snapshot.compare_session_files(
                [reference])
            expect(ok and probe_detail == "",
                   f"as the wrapper's own short circuit already said "
                   f"(got {ok}, {probe_detail!r})")
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def test_the_report_is_json_the_consumer_can_actually_read() -> None:
    print("\n-- the helper's report is a JSON object with the documented keys")
    reference, divergent = _two_distinct_fixtures()
    tmp = Path(tempfile.mkdtemp(prefix="compare_shape_"))
    try:
        paths = _generations(tmp, reference, divergent, divergent_index=1)
        _outcome, report, _detail = codec.compare_session_snapshots(paths)
        expect(isinstance(report, dict), f"a JSON object (got {report!r})")
        # Round-tripped rather than eyeballed: `diff_pair` reads these
        # keys by name, so a renamed field must fail here.
        expect(set(report or {}) >= {"outcome", "reference",
                                     "snapshotDiffers",
                                     "luaComponentDiffers"},
               f"carrying every key the consumer reads (got "
               f"{sorted(report or {})})")
        expect(json.loads(json.dumps(report)) == report,
               "and nothing in it that will not round-trip through JSON")
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def test_the_two_halves_of_the_comparison_are_reported_separately() -> None:
    print("\n-- snapshot equality and lua payload equality are separate "
          "answers, and both are really computed")
    reference, _divergent = _two_distinct_fixtures()

    # Direction one: a pair that differs on the SNAPSHOT half alone. A
    # `luaComponentDiffers` that merely mirrored the snapshot answer
    # would name this fixture here, so this is what proves the two are
    # computed independently.
    snapshot_only = _divergent_on(reference, "snapshotDiffers",
                                  "luaComponentDiffers")
    expect(snapshot_only is not None,
           f"the tracked corpus holds a fixture differing from "
           f"{reference.name} on the snapshot half ALONE -- without one, a "
           f"lua answer that simply copied the snapshot answer would pass "
           f"every member here")
    if snapshot_only is not None:
        outcome, report, _detail = _compare([reference, snapshot_only])
        expect(outcome == codec.COMPARE_MISMATCH,
               f"{snapshot_only.name} is still a mismatch (got {outcome!r})")
        expect((report or {}).get("snapshotDiffers") == [str(snapshot_only)],
               f"named on the snapshot half (got {report!r})")
        expect((report or {}).get("luaComponentDiffers") == [],
               f"and NOT on the lua half, whose payloads really do match "
               f"(got {report!r})")

    # Direction two: a pair that differs on BOTH. This is what proves the
    # lua comparison EXISTS -- dropping it altogether would leave the
    # answer above unchanged, and would leave this list empty.
    both = _divergent_on_both(reference)
    expect(both is not None,
           f"and one differing from {reference.name} on both halves -- "
           f"without one, a comparison that dropped the lua payload check "
           f"entirely would still pass every member here")
    if both is not None:
        outcome, report, _detail = _compare([reference, both])
        expect((report or {}).get("luaComponentDiffers") == [str(both)],
               f"whose lua payload difference is really reported "
               f"(got {report!r})")
        expect((report or {}).get("snapshotDiffers") == [str(both)],
               f"beside its snapshot one (got {report!r})")

    # The mirror: the reference against itself is equal on both halves, so
    # an empty list above is not simply what this always answers.
    outcome, report, _detail = _compare([reference, reference])
    expect(outcome == codec.COMPARE_OK,
           f"while the same fixture twice matches on both (got {outcome!r})")


def test_a_save_that_cannot_be_READ_is_a_decode_failure_naming_it() -> None:
    print("\n-- a generation that was never written is DECODE_FAILED, not "
          "an uncaught exception")
    reference, divergent = _two_distinct_fixtures()
    tmp = Path(tempfile.mkdtemp(prefix="compare_missing_"))
    try:
        paths = _generations(tmp, reference, divergent, divergent_index=9)
        # A path the comparison is handed but that is not there: a save
        # the probe believed it wrote, or one cleaned up early. The
        # helper reads its inputs with `BS.readFile`, which throws.
        paths[1].unlink()
        outcome, report, detail = _compare(paths)
        expect(outcome == codec.COMPARE_DECODE_FAILED,
               f"it is a decode failure, which describes the saves -- not "
               f"COMPARE_ERROR, which would describe the toolchain "
               f"(got {outcome!r}: {detail!r})")
        errors = (report or {}).get("decodeErrors")
        expect(isinstance(errors, list) and len(errors) == 1
               and errors[0].get("path") == str(paths[1]),
               f"naming the file it could not read (got {report!r})")
        expect(isinstance(errors, list) and errors
               and "read:" in str(errors[0].get("error") or ""),
               f"and saying that reading it, not decoding it, is what "
               f"failed (got {report!r})")
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


#: This owner's members, in the run order the façade concatenates
#: (issue #2073 requirement 12).
TESTS = [
    test_identical_generations_compare_ok,
    test_a_mismatch_names_the_generation_that_first_diverged,
    test_a_gen4_divergence_is_reported_too,
    test_an_undecodable_generation_is_a_decode_failure_not_a_mismatch,
    test_a_helper_that_cannot_be_run_is_not_a_verdict_about_the_saves,
    test_a_marker_inside_a_fixture_path_is_not_read_as_an_outcome,
    test_an_answer_this_bridge_cannot_corroborate_is_an_error,
    test_the_diagnostic_pair_falls_back_without_a_usable_report,
    test_fewer_than_two_paths_never_reaches_the_helper,
    test_the_report_is_json_the_consumer_can_actually_read,
    test_the_two_halves_of_the_comparison_are_reported_separately,
    test_a_save_that_cannot_be_READ_is_a_decode_failure_naming_it,
]
